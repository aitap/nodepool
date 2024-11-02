.do_connect <- function(host, port) {
	conn <- socketConnection(host, port, blocking = TRUE, open = 'a+b')
	# This is the only place where we do unprotected serialize().
	# It's better to let a fresh connection fail right away.
	serialize(
		list(type = 'HELO', format = if (getRversion() < '3.5.0') 2 else 3),
		conn
	)
	conn
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
# for the reply of type OK, then send one request of type EXEC and wait
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
		tasks = 'list'
	),
	methods = list(
		log = function(format, ...) message(
			'[', format(Sys.time()), '] ',
			sprintf(format, ...)
		),
		initialize = function(host, port) {
			stopifnot(
				is.character(host), length(host) == 1,
				is.numeric(port), length(port) == 1,
				port %% 1 == 0, port %in% 1:65535
			)
			.self$host <- host
			.self$port <- port
			.self$socket <- NULL
			.self$tasks <- list()
			if (!try_connect(host, port))
				stop("Initial connection attempt failed")
		},
		finalize = function() { # never leak unclosed connections
			if (!is.null(socket)) disconnect()
		},
		disconnect = function() {
			stopifnot(!is.null(socket))
			close(socket)
			.self$socket <- NULL
		},
		# leaves disconnected on failure
		try_connect = function() {
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
		connected = function() !is.null(socket),
		# send, recv disconnect on failure
		send = function(payload) tryCatch({
			while (!socketSelect(list(socket), TRUE)) {}
			serialize(payload, socket)
			TRUE
		}, error = function(e) {
			disconnect()
			FALSE
		}),
		recv = function() tryCatch({
			while (!socketSelect(list(socket))) {}
			ret <- unserialize(socket)
			list(ret)
		}, error = function(e) {
			disconnect()
			NULL
		}),
		expect = function(type) {
			val <- recv()
			if (is.null(val)) return(NULL) # disconnected
			if (identical(val[[1]]$type, type)) {
				val
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
			if (
				send(list(type = 'REQUEST')) &&
				!is.null(expect('PROCEED')) &&
				send(list(
					type = 'EXEC', tag = tag, fun = fun, args = args
				))
			) TRUE else FALSE
		},
		submit = function(tag, fun, args) {
			.self$tasks <- c(.self$tasks, list(
				list(tag = tag, fun = fun, args = args)
			))
			if (connected() && do_submit_one(tag, fun, args)) return()
			# disconnected! start from scratch
			repeat {
				log("reconnecting to resubmit %d task(s)", length(tasks))
				if (!try_connect()) next
				for (task in tasks)
					if (!do_submit_one(task$tag, task$fun, task$args))
						next
				break
			}
		}
	)
)

.makenode <- function(index, state) structure(list(
	index = index,
	state = state
), class = 'nodepool_node')

# [un]serialize() has a tentency to fail without an explanation.
# We might avoid a timeout by select()ing first.
.maybe_serialize <- function(con, data) tryCatch({
	socketSelect(list(con), TRUE)
	serialize(data, con)
	TRUE
}, error = function(e) FALSE)
.maybe_unserialize <- function(con) tryCatch({
	socketSelect(list(con))
	list(TRUE, unserialize(con))
}, error = function(e) list(FALSE))

# Establish the connection and submit all pending tasks
.reset_client <- function(state, why) tryCatch({
	message(format(Sys.time()), ': ', why, ', trying to restore')
	# If the user had close()d the connection, the following close()
	# will keep failing indefinitely. Catch the (unlikely but possible)
	# failure and try to make progress with a new connection.
	if (!is.null(state$conn)) try(close(state$conn))
	state$available <- FALSE
	state$conn <- NULL
	state$conn <- .do_connect(state$host, state$port)
	# FIXME: when resubmitting while resetting, these transfers go out unconfirmed
	# TODO: rewrite as a queue of unsent tasks and a function make_progress()
	for (task in state$byindex)
		if (!is.null(task) && !isTRUE(task$complete) && !.maybe_serialize(state$conn, task$task))
			return(FALSE)
	TRUE
}, error = function(e) FALSE)

.retry_serialize <- function(state, data) {
	while (!.maybe_serialize(state$conn, data))
		while (!.reset_client(state, 'failed to send')) {}
}

.retry_unserialize <- function(state) repeat {
	ret <- .maybe_unserialize(state$conn)
	if (ret[[1]]) return(ret[[2]])
	while (!.reset_client(state, 'failed to receive')) {}
}

.submitOne <- function(state, data) {
	# Wait until pool is able to handle the task
	if (!state$available) {
		.retry_serialize(state, list(type = 'REQUEST'))
		while (!state$available) .recvOne(state)
	}
	.retry_serialize(state, data)
	state$available <- FALSE
}

.warnedOnce <- new.env(parent = emptyenv())

# Remember the index of the node corresponding to this task
sendData.nodepool_node <- function(node, data) {
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

	if (identical(data$type, 'EXEC')) {
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
		.submitOne(node$state, data)
	} else .retry_serialize(node$state, data)
}

# Read one response. Look up and repair the tag. Return.
.recvOne <- function(state) {
	value <- .retry_unserialize(state)

	switch(value$type,
		VALUE = {
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
		},
		PROCEED = {
			state$available <- TRUE

			invisible()
		}
	)
}

recvData.nodepool_node <- function(node) {
	# Receive and remember responses as they come
	while (!node$state$byindex[[node$index]]$complete)
		.recvOne(node$state)

	value <- node$state$byindex[[node$index]]$value
	on.exit(node$state$byindex[node$index] <- list(NULL))
	value
}

recvOneData.nodepool_cluster <- function(cl) repeat {
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
}

stopCluster.nodepool_cluster <- function(cl, ...) {
	# NOTE: this makes it impossible to subclass nodes
	sendData.nodepool_node(cl[[1]], list(type = 'DONE'))
	close(cl)
}

close.nodepool_cluster <- function(con, ...) {
	if (!is.null(con[[1]]$state$conn)) close(con[[1]]$state$conn)
	con[[1]]$state$conn <- NULL
}

.do_connect <- function(host, port) {
	conn <- socketConnection(host, port, blocking = TRUE, open = 'a+b')
	# This is the only place where we do unprotected serialize().
	# It's better to let a fresh connection fail right away.
	serialize(
		list(type = 'HELO', format = if (getRversion() < '3.5.0') 2 else 3),
		conn
	)
	conn
}

pool_connect <- function(host, port, length = 0x80) {
	conn <- .do_connect(host, port)
	state <- list2env(
		list(
			conn = conn,
			byindex = vector('list', length),
			host = host,
			port = port,
			available = FALSE
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
	cat(
		'<Connection to the pool server at ',
		x[[1]]$state$host, ':', x[[1]]$state$port,
		if (!is.null(pid <- attr(x, 'pid'))) paste0(' (PID ', pid, ')'),
		if ((nodes <- length(attr(x, 'nodepids'))) > 0)
			paste(' with', nodes, 'local node[s]'),
		if (inherits(try(isOpen(x[[1]]$state$conn), TRUE), 'try-error'))
			', currently closed',
		'>\n', sep = ''
	)
	invisible(x)
}
