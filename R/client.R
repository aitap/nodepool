pool_connect <- function(host, port)
	socketConnection(host, port, blocking = TRUE, open = 'a+b')

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
		if (!value$success) { stop(value$value) }
		ret[[value$tag]] <- value$value
	}
	ret
}
