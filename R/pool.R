setClass('Connection')

mPool <- setRefClass('Pool',
	fields = list(
		server = 'Connection',
		portnum = 'integer',

		clients = 'list', # actually list<ClientConnection>
		nodes = 'list',   # actually list<NodeConnection>
		tasks = 'list',    # actually list<Task>
		# would like these to be mutable

		running = 'logical',
		verbose = 'logical'
	),
	methods = list(
		listen = function(port) {
			if (is.null(port)) repeat {
				# must guess a working port
				port <- floor(runif(1, 1024, 65536))
				socket <- try(serverSocket(port), TRUE)
				if (!inherits(socket, 'try-error')) break
			} else {
				stopifnot(
					is.numeric(port), length(port) == 1, port == round(port),
					port > 0, port < 65536
				)
				socket <- serverSocket(port)
			}
			.self$portnum <- as.integer(port)
			.self$server <- mServerConnection(socket = socket, pool = .self)
		},
		initialize = function(port, verbose = TRUE) {
			listen(port)
			.self$clients <- list()
			.self$nodes <- list()
			.self$tasks <- list()
			.self$running <- TRUE
			.self$verbose <- TRUE
		},
		finalize = function() {
			.self$running <- FALSE
			server$finalize()
			for (cl in clients) cl$finalize()
			for (n in nodes) n$finalize()
			.self$clients <- list()
			.self$nodes <- list()
			.self$tasks <- list()
		},
		show = function() cat(
			'<Pool(port=', portnum, '): ',
			length(clients), ' client(s), ',
			length(nodes),   ' node(s), ',
			length(tasks), ' task(s), ',
			if (running) 'running' else 'STOPPED',
			'>\n', sep = ''
		),
		process_one_event = function() {
			if (.self$verbose) show()
			connections <- c(list(server), clients, nodes)
			to_write <- vapply(connections, function(s) s$need_write(), FALSE)
			sockets <- lapply(connections, function(s) s$socket)

			# some sockets just don't want to be bothered at all for now
			mask <- !is.na(to_write)
			connections <- connections[mask]
			sockets <- sockets[mask]
			to_write <- to_write[mask]

			events <- socketSelect(sockets, to_write)
			if (.self$verbose) print(noquote(rbind(
				ifelse(to_write, 'write', 'read'),
				c(
					'server',
					rep('client', length(clients)),
					rep('node', length(nodes))
				),
				ifelse(events, 'event', '-')
			)))
			# guess what, sample(<length-1 whole number>) will act like sample.int!
			which.events <- which(events)
			idx <- which.events[sample.int(length(which.events), 1)]
			if (.self$verbose) cat('Event at socket ', idx, '\n')
			connections[[idx]]$process_event()
		},
		add_connection = function(client) {
			# Connections start out as clients and may later transition
			# to being nodes if they send a special message.
			.self$clients <- c(clients, list(client))
		},
		remove_node = function(node) {
			node$finalize() # explicitly clean up right away
			# If was working on a task, must give it to a different node
			if (!is.null(node$task))
				.self$tasks <- c(tasks, list(node$task))
			keep <- !vapply(nodes, identical, FALSE, node)
			.self$nodes <- nodes[keep]
		},
		remove_client = function(client) {
			client$finalize()
			# Remove the client's tasks
			keep <- !vapply(tasks, function(task) identical(task$client, client), FALSE)
			.self$tasks <- tasks[keep]
			# Remove the client
			keep <- !vapply(clients, identical, FALSE, client)
			.self$clients <- clients[keep]
		},
		add_task = function(task) {
			.self$tasks <- c(tasks, list(task))
		},
		pop_task = function() {
			stopifnot(length(tasks) > 0)
			ret <- tasks[[1]]
			.self$tasks <- tasks[-1]
			ret
		},
		make_node = function(client) {
			sock <- client$socket
			client$socket <- NULL
			remove_client(client)
			.self$nodes <- c(nodes, list(mNodeConnection(sock, .self, client$format)))
		},
		halt = function() {
			# disconnect the nodes gracefully
			# TODO: the clients may need a warning too, but (1) they
			# don't handle it yet and (2) they are the one(s?)
			# initiating the shutdown, so all but one ought to have
			# disconnected already.
			for (node in nodes) try(
				node$send(list(type = 'DONE')),
				TRUE
			)
			.self$running <- FALSE
		},
		run = function() while (running) process_one_event()
	)
)
mPool$lock(c('portnum', 'server'))

setOldClass(c('servsockconn', 'sockconn'))
setClassUnion('optional_sockconn', c('servsockconn', 'sockconn', 'NULL'))
setClassUnion('optional_Pool', c('Pool', 'NULL'))
setRefClass('ConnectionBase',
	fields = list(
		socket = 'optional_sockconn',
		pool = 'optional_Pool',
		format = 'numeric'
	),
	contains = c('Connection', 'VIRTUAL'),
	methods = list(
		# We're more or less guaranteed to be called with an empty
		# argument list, so we have to allow empty Connection objects
		# (socket = NULL), even if they are otherwise useless.
		initialize = function(socket = NULL, pool = NULL, format = 2) {
			stopifnot(missing(socket) == missing(pool))
			.self$socket <- socket
			.self$pool <- pool
			.self$format <- format
		},
		finalize = function() if (!is.null(socket)) {
			close(socket)
			.self$socket <- NULL
		},
		# Used for select().
		# Return NA if not ready to read or write.
		need_write = function() FALSE,
		# Subclasses *must* define a process_event() method that would
		# obtain the event however it can and call the referenced Pool
		# object to handle it. It will be called if socketSelect()
		# returns the corresponding socket as available.
		send = function(object)
			serialize(object, socket, version = format)
	)
)

mServerConnection <- setRefClass('ServerConnection',
	contains = 'ConnectionBase',
	methods = list(
		process_event = function() try({
			if (pool$verbose) writeLines('Trying to accept new connection')
			# Yes, accept() may fail
			pool$add_connection(mClientConnection(
				socketAccept(socket, open = 'a+b', blocking = TRUE), pool
			))
			if (pool$verbose) writeLines('Accepted new connection')
		})
	)
)

mClientConnection <- setRefClass('ClientConnection',
	fields = list(
		# Data to be sent to the other side of the connection.
		# To be used for already computed tasks.
		results = 'list'
	),
	contains = 'ConnectionBase',
	methods = list(
		# Write completed tasks back to the client; otherwise ask for
		# more.
		need_write = function() length(results) > 0,
		process_event = function() tryCatch(
			if (need_write()) {
				if (pool$verbose) writeLines('Sending results to client')
				send(results[[1]])
				# if this fails, the whole client is deleted together with the tasks
				.self$results <- results[-1]
				if (pool$verbose) writeLines('Sent results to client')
			} else {
				msg <- unserialize(socket)
				if (pool$verbose) cat('Client message of type ', msg$type, '\n')
				switch(msg$type,
					EXEC = pool$add_task(mTask(
						client = .self,
						tag = msg$data$tag,
						payload = msg
					)),
					NODE = pool$make_node(.self),
					DONE = pool$halt(),
					HELO = {
						f <- msg$format
						if (
							is.atomic(f) && length(f) == 1 &&
							f %in% 2:3
						) .self$format <- f
					}
				)
			},
			# sockets being closed will typically become "readable" and
			# then fail to read
			error = function(e) pool$remove_client(.self)
		),
		add_result = function(payload)
			.self$results <- c(results, list(payload))
	)
)

mTask <- setRefClass('Task',
	fields = list(
		client = 'ClientConnection',
		tag = 'ANY',
		payload = 'ANY'
	)
)
setClassUnion('optional_Task', c('Task', 'NULL'))

mNodeConnection <- setRefClass('NodeConnection',
	fields = list(
		task = 'optional_Task'
	),
	contains = 'ConnectionBase',
	methods = list(
		# If a node has a pending task, we're waiting for it to send the
		# task result back to us.
		# If the queue is not empty, we need to submit new jobs.
		# Otherwise, let's default to readable (we have nothing to
		# write).
		need_write = function()
			if (!is.null(task)) FALSE else length(pool$tasks) > 0,
		process_event = function() tryCatch(
			if (need_write()) {
				if (pool$verbose) writeLines('Sending task to node')
				.self$task <- pool$pop_task()
				send(task$payload)
				if (pool$verbose) writeLines('Sent task to node')
			} else {
				if (pool$verbose) writeLines('Reading result from node')
				msg <- unserialize(socket)
				if (pool$verbose) cat('Read ', msg$type, ' from node\n')
				switch(msg$type,
					VALUE = {
						msg$tag <- task$tag
						task$client$add_result(msg)
					}
				)
				.self$task <- NULL
			},
			error = function(e) pool$remove_node(.self)
		)
	)
)

run_pool <- function(port = NULL, background = FALSE, nodes = 0, verbose = TRUE, ...) {
	stopifnot(
		length(nodes) == 1,
		nodes == round(nodes),
		'Need R >= 4.0 in order to start a pool' = exists('serverSocket', baseenv())
	)

	if (!background) {
		stopifnot(
			!is.null(port),
			length(list(...)) == 0
		)
		pool <- mPool(port, verbose = verbose)
		for (i in seq_len(nodes)) run_node('localhost', port, TRUE)
		# Prevent the un-garbage-collected serverSocket from lingering open and
		# preventing another pool from starting later.
		on.exit(pool$finalize())
		return(pool$run())
	}

	ret <- Rscript_payload(
		bquote({
			.pool <- loadNamespace('nodepool')$mPool(.(port), .(verbose))
			c(.pool$portnum, Sys.getpid())
		}),
		quote(.pool$run())
	)
	nodes <- replicate(nodes, run_node('localhost', ret[1], TRUE), FALSE)
	structure(
		pool_connect('localhost', ret[1], ...),
		pid = ret[2],
		nodepids = nodes
	)
}
