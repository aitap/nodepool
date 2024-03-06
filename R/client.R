.makenode <- function(conn, state, index) structure(list(
	conn = conn,
	state = state,
	index = index
), class = 'nodepool_node')

# Remember the index of the node corresponding to this task
sendData.nodepool_node <- function(node, data) {
	if (identical(data$type, 'EXEC')) {
		# Since the results may arrive out of order, mark every job with
		# the index of the node it has been submitted against.
		orig_tag <- data$data$tag
		data$data$tag <- node$index
		serialize(data, node$conn)
		node$state$byindex[[node$index]] <- list(
			tag = orig_tag,
			complete = FALSE,
			value = NULL
		)
	} else serialize(data, node$conn)
}

# Read one response. Look up and repair the tag. Return.
.recvOne <- function(conn, state) {
	value <- unserialize(conn)

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
		value <- .recvOne(node$conn, node$state)
		node$state$byindex[[value$node]]$value <- value$value
		node$state$byindex[[value$node]]$complete <- TRUE
	}
	value <- node$state$byindex[[node$index]]$value
	on.exit(node$state$byindex[node$index] <- list(NULL))
	value
}

recvOneData.nodepool_cluster <- function(cl) {
	.recvOne(cl[[1]]$conn, cl[[1]]$state)
}

stopCluster.nodepool_cluster <- function(cl, ...) {
	# TODO: use 'DONE' like in parallel itself
	parallel:::sendData(cl[[1]], list(type = 'HALT'))
	close(cl)
}

close.nodepool_cluster <- function(con, ...) close(con[[1]]$conn)

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
	state$byindex <- vector('list', length)
	structure(
		lapply(
			seq_len(length),
			function(index) .makenode(conn, state, index)
		),
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
		if (inherits(try(isOpen(x[[1]]$conn), TRUE), 'try-error'))
			', currently closed',
		'>\n', sep = ''
	)
	invisible(x)
}
