.nodeloop <- function(socket, env = new.env(parent = globalenv())) repeat {
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
		},
		HALT = break
	)
}

.run_node <- function(host, port) {
	socket <- pool_connect(host, port)
	on.exit(close(socket), add = TRUE)

	wd <- tempfile('nodewd')
	dir.create(wd)
	oldwd <- setwd(wd)
	on.exit(setwd(oldwd), add = TRUE)

	lbr <- tempfile('nodelib')
	dir.create(lbr)
	oldlib <- .libPaths()
	# any install.packages() calls will end up in the temporary library,
	# but any already-installed packages can be loaded from libraries
	# previously set up
	.libPaths(c(lbr, oldlib))
	on.exit(.libPaths(oldlib), add = TRUE)

	serialize(list(type = 'NODE'), socket)
	.nodeloop(socket)
}

run_node <- function(host, port, background = FALSE) {
	# ::: raises a NOTE, so we're using a different idiom
	if (background) {
		ret <- Rscript_payload(
			quote(Sys.getpid()),
			bquote(loadNamespace('nodepool')$.run_node(.(host), .(port)))
		)
		structure(ret, class = 'run_node')
	} else Rscript(bquote(loadNamespace('nodepool')$.run_node(.(host), .(port))), TRUE)

}

print.run_node <- function(x, ...) {
	stopifnot(length(list(...)) == 0)
	cat('Node started on PID', x, '\n')
	invisible(x)
}
