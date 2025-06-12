log <- function(format, ...) message(
	'[', format(Sys.time()), '] ',
	sprintf(format, ...)
)

compressed_task_fun <- function(type) {
	force(type)
	function(payload) {
		payload <- unserialize(memDecompress(payload, type))
		payload <- do.call(payload$fun, payload$args, TRUE)
		payload <- serialize(payload, NULL, version = 2)
		memCompress(payload, type)
	}
}

# The client protocol
# ===================
# All requests are initiated by the client and answered by the pool.
# Requests are serialized named lists with the 'type' field, a single
# string, specifying the request type and other arbitrary fields.
#
# The first request is always of type HELO and following fields:
# - format:   3L, recommended
#          or 2L, if the client doesn't speak RDS format 3 (R < 3.5)
# - protocol: 1L, required
#
# To submit a task, the client must send a request of type REQUEST, wait
# for the reply of type PROCEED, then send one request of type EXEC and wait
# for the reply of type OK. The EXEC request contains the following
# fields:
# - tag:  will be returned together with the result
# - fun:  the function to execute on the node
# - args: the arguments to give to the function
#
# To receive a result, the client must send a request of type RECEIVE
# and wait for the reply of type VALUE with the following fields:
# - value:   the result of do.call(fun, args, quote = TRUE), if successful
#         or the value of the error object, if signalled
# - success: TRUE, if completed without singalling errors
#         or FALSE, if an error was signalled and caught
# - time:    an object of class proc_time containing the time it took to
#            evaluate the requested expression
# - tag:     the original tag submitted in the EXEC request.

mPoolClient <- setRefClass('PoolClient',
	fields = list(
		host = 'character',
		port = 'numeric',
		socket = 'optional_sockconn',
		tasks = 'list',
		compress = 'character'
	),
	methods = list(
		show = function()
			cat(sprintf(
				"Connection to pool at %s:%d, %d task(s) in queue, currently %s\n",
				host, port, length(tasks), if (connected()) 'active' else 'inactive'
			)),
		initialize = function(host, port, compress = 'none') {
			"Connects to the pool. 'host' must be a string specifying
			the address of the pool server. 'port' must be a valid TCP
			port number. If the first attempt to connect and exchange
			messages, the operation fails. Subsequent failures will be
			automatically retried."
			stopifnot(
				is.character(host), length(host) == 1,
				is.numeric(port), length(port) == 1,
				port %% 1 == 0, port %in% 1:65535,
				compress %in% c('none', 'gzip', 'bzip2', 'xz')
			)
			.self$host <- host
			.self$port <- port
			.self$socket <- NULL
			.self$tasks <- list()
			.self$compress <- compress
			if (!try_connect())
				stop("Initial connection attempt failed")
		},
		connected = function() !is.null(socket),
		disconnect = function() {
			stopifnot(!is.null(socket))
			close(socket)
			.self$socket <- NULL
		},
		finalize = function() if (connected()) disconnect(),
		try_connect = function() {
			"Precondition: connection not established (socket is NULL).
			Returns TRUE if the connection and initial message exchange
			succeeded. Otherwise returns FALSE and keeps socket = NULL."
			stopifnot(is.null(socket))
			tryCatch({
				.self$socket <- socketConnection(
					host, port, blocking = TRUE, open = 'a+b'
				)
				send(list(
					type = 'HELO',
					format = if (getRversion() < '3.5.0') 2L else 3L,
					protocol = 1L
				))
			}, error = function(e) {
				if (!is.null(.self$socket)) disconnect()
				FALSE
			})
		},
		send = function(payload) {
			"Precondition: connection established. Tries to send the
			'payload' to the pool. Returns TRUE if sending succeeded.
			Otherwise disconnects and returns FALSE."
			stopifnot(connected())
			tryCatch({
				while (!socketSelect(list(socket), TRUE)) {}
				serialize(payload, socket)
				TRUE
			}, error = function(e) {
				disconnect()
				FALSE
			})
		},
		recv = function() {
			"Precondition: connection established. Tries to receive one
			message from the pool. Returns a one-element list containing
			the message if succeeded. Otherwise disconnects and returns
			NULL."
			stopifnot(connected())
			tryCatch({
				while (!socketSelect(list(socket))) {}
				ret <- unserialize(socket)
				list(ret)
			}, error = function(e) {
				disconnect()
				NULL
			})
		},
		expect = function(type) {
			"Precondition: connection established. Tries to receive a
			message of the given type from the pool. Returns the message
			if succeeded. Otherwise logs the protocol error, disconnects
			and returns NULL."
			stopifnot(connected())
			val <- recv()
			if (is.null(val)) return(NULL) # disconnected
			if (identical(val[[1]]$type, type)) {
				val[[1]]
			} else {
				log(
					'protocol error! expected %s, got %s',
					type, val$type
				)
				disconnect()
				NULL
			}
		},
		do_submit_one = function(tag, fun, args) {
			"Preconditions: connection established. Tries to send a task
			to run do.call(fun, args) to the pool. Returns TRUE if
			succeeded. Otherwise disconnects and returns NULL."
			stopifnot(connected())
			send(list(type = 'REQUEST')) &&
			!is.null(expect('OK')) &&
			send(list(
				type = 'EXEC', tag = tag, fun = fun, args = args
			)) &&
			!is.null(expect('OK'))
		},
		reconnect_and_resubmit = function() repeat {
			"Precondition: connection previously failed (socket is
			NULL). Logs each attempt to reconnect. Returns after the
			connection is established and all queued tasks are
			submitted."
			stopifnot(!connected())
			log("reconnecting to resubmit %d task(s)", length(tasks))
			if (!try_connect()) next
			for (task in tasks)
				if (!do_submit_one(task$tag, task$fun, task$args))
					break
			# if not connected, last do_submit_one() must have failed
			if (connected()) break
		},
		submit = function(tag, fun, args) {
			"Precondition: 'tag' must not be already present among
			submitted tasks. Puts the task in the queue and tries to
			submit it to the pool. Returns after successfully submitting
			the task, possibly reconnecting (and resubmitting
			everything) in the process."
			stopifnot(
				`Must not submit an already submitted tag` = all(vapply(
					tasks, function(task, tag) !identical(tag, task$tag),
					FALSE, tag
				))
			)
			if (compress != 'none') {
				args <- list(memCompress(
					serialize(
						list(fun = fun, args = args),
						NULL,
						version = 2
					),
					compress
				))
				fun <- compressed_task_fun(compress)
			}
			.self$tasks <- c(.self$tasks, list(
				list(tag = tag, fun = fun, args = args)
			))
			if (connected() && do_submit_one(tag, fun, args)) return()
			# disconnected! start from scratch
			reconnect_and_resubmit()
		},
		abandon_tasks = function() {
			"Clears the list of pending tasks and disconnects to signal
			the server to do the same."
			# can't introduce a command to do that because we may have
			# already sent a command and may be waiting for a reply
			.self$tasks <- list()
			if (connected()) disconnect()
		},
		halt = function() {
			"Sends a message to the pool to stop all nodes and cease
			operations."
			repeat {
				if (connected() && send(list(type = 'DONE'))) return()
				try_connect()
			}
		},
		get_result = function() {
			"Precondition: must have previously submitted a task.
			Receives the value for one of the queued tasks, possibly
			after reconnecting and resubmitting them all. The format of
			the value is a named list with the following elements:
			- 'value': the value of the function call if it succeeed, or
			the value of the signalled error condition if it failed.
			- 'success': logical scalar indicating whether the
			evaluation succeeded without raising an error.
			- 'time': the time it took to evaluate the function call.
			- 'tag': the original 'tag' given to submit()."
			repeat {
				stopifnot(`Must only receive after having submitted a task`=length(tasks) > 0)
				if (
					connected() &&
					send(list(type = 'RECEIVE')) &&
					!is.null(ret <- expect('VALUE'))
				) break
				reconnect_and_resubmit()
			}
			.self$tasks <- Filter(
				function(task) !identical(task$tag, ret$tag),
				.self$tasks
			)
			if (compress != 'none' && ret$success)
				ret$value <- unserialize(
					memDecompress(ret$value, compress)
				)
			ret
		}
	)
)

.makenode <- function(index, state) structure(list(
	index = index,
	state = state
), class = 'nodepool_node')

.abandon_tasks <- function(state) {
	state$conn$abandon_tasks()
	state$byindex <- vector('list', length(state$byindex))
}

.warnedOnce <- new.env(parent = emptyenv())
sendData.nodepool_node <- function(node, data) withCallingHandlers({
	if (
		sys.nframe() >= 4 &&
		identical(sys.function(-4), parallel::clusterCall) &&
		!isTRUE(.warnedOnce$clusterCall)
	) {
		.warnedOnce$clusterCall <- TRUE
		warning(
			'Due to dynamic task distribution and the potential for ',
			'nodes to leave and rejoin, clusterCall() will not work ',
			'well with nodepool clusters, sorry.', call. = NULL
		)
	}

	stopifnot(identical(data$type, 'EXEC'))
	# Since the results may arrive out of order, mark every job with
	# the index of the node it has been submitted against.
	orig_tag <- data$data$tag
	data$data$tag <- node$index
	node$state$byindex[[node$index]] <- list(
		tag = orig_tag,
		complete = FALSE,
		value = NULL,
		task = data
	)
	node$state$conn$submit(
		node$index, data$data$fun, data$data$args
	)
},
	interrupt = function(e) .abandon_tasks(node$state),
	error     = function(e) .abandon_tasks(node$state)
)

# Read one response. Look up and repair the tag. Return.
.recvOne <- function(state) {
	value <- state$conn$get_result()

	stopifnot(
		'Internal error: received a job result without a tag' = !is.null(value$tag),
		'Internal error: received a job result with an invalid tag' =
			is.numeric(value$tag) && length(value$tag) == 1 &&
			round(value$tag) == value$tag
	)

	index <- value$tag
	value$tag <- state$byindex[[index]]$tag
	state$byindex[[index]]$value <- value
	state$byindex[[index]]$complete <- TRUE

	index
}

recvData.nodepool_node <- function(node) withCallingHandlers({
	# Receive and remember responses as they come
	while (!node$state$byindex[[node$index]]$complete)
		.recvOne(node$state)

	value <- node$state$byindex[[node$index]]$value
	on.exit(node$state$byindex[node$index] <- list(NULL))
	value
},
	interrupt = function(e) .abandon_tasks(node$state),
	error     = function(e) .abandon_tasks(node$state)
)

recvOneData.nodepool_cluster <- function(cl) withCallingHandlers(repeat {
	# anything already received?
	complete <- vapply(cl[[1]]$state$byindex, function(x) isTRUE(x$complete), FALSE)
	if (any(complete)) {
		index <- which.max(complete)
		return(list(
			node = index,
			value = recvData.nodepool_node(cl[[index]])
		))
	}
	# try to receive more
	.recvOne(cl[[1]]$state)
},
	interrupt = function(e) .abandon_tasks(cl[[1]]$state),
	error     = function(e) .abandon_tasks(cl[[1]]$state)
)

stopCluster.nodepool_cluster <- function(cl, ...) {
	# NOTE: this makes it impossible to subclass nodes
	cl[[1]]$state$conn$halt()
	close(cl)
}

close.nodepool_cluster <- function(con, ...) {
	.abandon_tasks(con[[1]]$state)
}

pool_connect <- function(host, port, length = 0x80, compress = 'none') {
	conn <- mPoolClient(host, port, compress)
	state <- list2env(
		list(
			conn = conn,
			byindex = vector('list', length)
		),
		parent = emptyenv()
	)
	structure(
		lapply(seq_len(length), .makenode, state = state),
		class = c('nodepool_cluster', 'cluster'),
		host = host,
		port = port
	)
}

print.nodepool_cluster <- function(x, ...) {
	x[[1]]$state$conn$show()
	cat(
		if (!is.null(pid <- attr(x, 'pid'))) paste0(' (PID ', pid, ')'),
		if ((nodes <- length(attr(x, 'nodepids'))) > 0)
			paste(' with', nodes, 'local node[s]'),
		if (!x[[1]]$state$conn$connected())
			', currently closed',
		'\n', sep = ''
	)
	invisible(x)
}
