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
	if (!is.null(state$conn)) close(state$conn)
	state$available <- FALSE
	state$conn <- NULL
	state$conn <- .do_connect(state$host, state$port)
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

# Remember the index of the node corresponding to this task
sendData.nodepool_node <- function(node, data) {
	if (identical(data$type, 'EXEC')) {
		# Wait until pool is able to handle the task
		node$state$available <- FALSE
		.retry_serialize(node$state, list(type = 'REQUEST'))
		while (!node$state$available) .recvOne(node$state)

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

		.retry_serialize(node$state, data)
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
	index <- .recvOne(cl[[1]]$state)
	if (!is.null(index)) return(list(
		node = index,
		value = recvData.nodepool_node(cl[[index]])
	))
}

stopCluster.nodepool_cluster <- function(cl, ...) {
	# NOTE: this makes it impossible to subclass nodes
	sendData.nodepool_node(cl[[1]], list(type = 'DONE'))
	close(cl)
}

close.nodepool_cluster <- function(con, ...)
	if (!is.null(con[[1]]$state$conn)) close(con[[1]]$state$conn)

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
