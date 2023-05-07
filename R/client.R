pool_connect <- function(host, port) {
	ret <- socketConnection(host, port, blocking = TRUE, open = 'a+b')
	attr(ret, 'host') <- host
	attr(ret, 'port') <- port
	class(ret) <- c('pool_connection', class(ret))
	ret
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
