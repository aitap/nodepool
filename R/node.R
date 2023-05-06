.run_node <- function(host, port) {
	socket <- socketConnection(host, port, blocking = TRUE, open = 'a+b')
	on.exit(close(socket), add = TRUE)

	wd <- tempfile('nodewd')
	dir.create(wd)
	oldwd <- setwd(wd)
	on.exit(setwd(oldwd), add = TRUE)

	lbr <- tempfile('nodelib')
	dir.create(lbr)
	oldlib <- .libPaths()
	.libPaths(lbr)
	on.exit(.libPaths(oldlib), add = TRUE)

	# FIXME: can we make an empty environment that's otherwise just like
	# the global environment and run the tasks there?
	env <- new.env(parent = loadNamespace('base'))

	serialize(list(type = 'NODE'), socket)
	repeat {
		# otherwise unserialize() eventually fails with a timeout
		socketSelect(list(socket))
		msg <- unserialize(socket)
		switch(msg$type,
			EXEC = {
				# adapted from the "parallel" package, GPL-2|GPL-3, (c) R Core Team
				success <- TRUE
				handler <- function(e) {
					success <<- FALSE
					e
				}
				t1 <- proc.time()
				value <- tryCatch(
					do.call(
						msg$data$fun, msg$data$args,
						quote = TRUE, envir = env
					),
					error = handler
				)
				t2 <- proc.time()
				value <- list(
					type = "VALUE", value = value, success = success,
					time = t2 - t1, tag = msg$data$tag
				)
				serialize(value, socket)
				rm(value)
			}
		)
		rm(msg)
	}
}

run_node <- function(host, port, background = FALSE) {
	if (!background) return(.run_node(host, port))

	ret <- Rscript_payload(
		quote(Sys.getpid()),
		bquote(nodepool::run_node(.(host), .(port), FALSE))
	)
	structure(ret, class = 'run_node')
}

print.run_node <- function(x, ...) {
	stopifnot(length(list(...)) == 0)
	cat('Node started on PID', x, '\n')
}
