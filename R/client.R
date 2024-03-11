.makenode <- function(index, state) structure(list(
	index = index,
	state = state
), class = 'nodepool_node')

# Remember the index of the node corresponding to this task
sendData.nodepool_node <- function(node, data) {
	# serialize() has a tentency to fail without an explanation.
	# We might avoid a timeout by select()ing first, but it's more
	# reliable to be able to reconnect automatically.
	socketSelect(list(node$state$conn), TRUE)
	if (identical(data$type, 'EXEC')) {
		# Since the results may arrive out of order, mark every job with
		# the index of the node it has been submitted against.
		orig_tag <- data$data$tag
		data$data$tag <- node$index
		serialize(data, node$state$conn)
		node$state$byindex[[node$index]] <- list(
			tag = orig_tag,
			complete = FALSE,
			value = NULL
		)
	} else serialize(data, node$state$conn)
}

# Read one response. Look up and repair the tag. Return.
.recvOne <- function(state) {
	# unserialize() will time out otherwise
	socketSelect(list(state$conn))
	value <- unserialize(state$conn)

	stopifnot(
		'Internal error: received a job result without a tag' = !is.null(value$tag),
		'Internal error: received a job result with an invalid tag' =
			is.numeric(value$tag) && length(value$tag) == 1 &&
			round(value$tag) == value$tag
	)

	index <- value$tag
	value$tag <- state$byindex[[index]]$tag
	list(node = index, value = value)
}

recvData.nodepool_node <- function(node) {
	# Receive and remember responses as they come
	while (!node$state$byindex[[node$index]]$complete) {
		value <- .recvOne(node$state)
		node$state$byindex[[value$node]]$value <- value$value
		node$state$byindex[[value$node]]$complete <- TRUE
	}
	value <- node$state$byindex[[node$index]]$value
	on.exit(node$state$byindex[node$index] <- list(NULL))
	value
}

recvOneData.nodepool_cluster <- function(cl) {
	.recvOne(cl[[1]]$state)
}

stopCluster.nodepool_cluster <- function(cl, ...) {
	# TODO: allow subclassing nodes
	sendData.nodepool_node(cl[[1]], list(type = 'DONE'))
	close(cl)
}

close.nodepool_cluster <- function(con, ...) close(con[[1]]$state$conn)

.do_connect <- function(host, port) {
	conn <- socketConnection(host, port, blocking = TRUE, open = 'a+b')
	serialize(
		list(type = 'HELO', format = if (getRversion() < '3.5.0') 2 else 3),
		conn
	)
	conn
}

pool_connect <- function(host, port, length = 0x80) {
	conn <- .do_connect(host, port)
	state <- new.env(parent = emptyenv())
	state$conn <- conn
	state$byindex <- vector('list', length)
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
		attr(x, 'host'), ':', attr(x, 'port'),
		if (!is.null(pid <- attr(x, 'pid'))) paste0(' (PID ', pid, ')'),
		if ((nodes <- length(attr(x, 'nodepids'))) > 0)
			paste(' with', nodes, 'local node[s]'),
		if (inherits(try(isOpen(x[[1]]$state$conn), TRUE), 'try-error'))
			', currently closed',
		'>\n', sep = ''
	)
	invisible(x)
}
