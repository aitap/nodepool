pool_connect <- function(host, port) {
	ret <- socketConnection(host, port, blocking = TRUE, open = 'a+b')
	attr(ret, 'host') <- host
	attr(ret, 'port') <- port
	class(ret) <- c('pool_connection', class(ret))
	serialize(
		list(type = 'HELO', format = if (getRversion() < '3.5.0') 2 else 3),
		ret
	)
	ret
}

print.pool_connection <- function(x, ...) {
	stopifnot(length(list(...)) == 0)
	cat(
		'<Connection to the pool server at ',
		attr(x, 'host'), ':', attr(x, 'port'),
		if (!is.null(pid <- attr(x, 'pid'))) paste0(' (PID ', pid, ')'),
		if ((nodes <- length(attr(x, 'nodepids'))) > 0)
			paste(' with', nodes, 'local node[s]'),
		if (inherits(try(isOpen(x), TRUE), 'try-error'))
			', currently closed',
		'>\n', sep = ''
	)
	invisible(x)
}

pool_halt <- function(pool) {
	serialize(list(type = 'HALT'), pool)
	close(pool)
}

lbapply <- function(x, fun, pool, ...) {
	for (i in seq_along(x))
		serialize(
			list(
				type = 'EXEC',
				data = list(fun = fun, args = list(x[[i]], ...), tag = i)
			),
			pool
		)
	ret <- list()
	for (i in seq_along(x)) {
		# the wait for results can be long, and unserialize() has been observed
		# to fail with a timeout
		socketSelect(list(pool))
		value <- unserialize(pool)
		if (!value$success) { warning(value$value) }
		ret[[value$tag]] <- value$value
	}
	ret
}
