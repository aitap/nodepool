startRscript <- function() {
	# NOTE: on Windows, this becomes a named pipe under \\.\pipe\, so
	# must unlink it once done
	path <- tempfile()

	bootstrap <- bquote(
		# the act of opening the FIFO acts as synchronisation
		f <- fifo(.(path), 'r', TRUE)
	)

	cmdline <- paste(
		shQuote(file.path(R.home('bin'), 'Rscript')),
		'-e',
		shQuote(paste(deparse(bootstrap), collapse = ' ')),
		'-e',
		shQuote('Sys.sleep(19)')
	)
	# So.
	# There is no function to create the FIFO, but R will do that for us
	# if opening a connection for writing.
	# On POSIX, opening a FIFO with O_NONBLOCK|O_WRONLY will fail unless
	# there's already a reader, and O_NONBLOCK|O_RDWR is undefined;
	# opening without O_NONBLOCK will block until both ends are
	# connected.
	# (What about Windows? No idea.)
	# We can't afford to block before the process is launched, so here's
	# a workaround:
	# (1) create a pipe but fail to open it
	try(suppressWarnings(close(fifo(path, 'w', FALSE))), TRUE)
	# (2) now that it exists, launch a child process
	# (note: pipes are always blocking, and so may be close())
	p <- pipe(cmdline, 'r')
	# (3) now it's safe for both processes to try opening the FIFO and
	# blocking for a bit
	f <- fifo(path, 'w', TRUE)
	list(
		path = path,
		pipe = p,
		fifo = f
	)
	# now, we "just" need (1) a way to check whether a pipe/FIFO is
	# readable/writable without blocking and (2) very preferrably, a way
	# for the child to monitor it being closed (and terminating, like on
	# EOF). tools::pskill is a tool of last resort because it uses
	# TerminateProcess and won't let R clean up
	# may have to use Tcl event loop?
}
