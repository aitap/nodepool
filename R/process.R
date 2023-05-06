# Start an Rscript process in the background and have it run a given
# expression. Cannot even return the PID, though it's mostly useless
# without a guarantee that it belongs to the same process.
Rscript <- function(expr) system2(
	file.path(R.home('bin'), 'Rscript'),
	c('-e', shQuote(paste(deparse(expr), collapse = '\n'))),
	wait = FALSE
)

# Start a background Rscript but have it return a value to the parent
# process, blocking until it's returned.
Rscript_payload <- function(value, continue) {
	# How to receive a piece of information from a background Rscript
	# and have it continue? Well...
	# We can't use pipe() (which is implemented using popen()) because
	# pclose() will wait until the child process terminates and we don't
	# want that. (Keeping the connection open is not worth the hassle.)
	# What we can do is pre-arrange to use a temporary named pipe.

	path <- tempfile('Rscriptfifo')
	# FIXME: does unlink() even work for named pipes on Windows?
	on.exit(unlink(path), add = TRUE)

	payload <- substitute({
		f <- fifo(path, 'w', TRUE)
		serialize(value, f, TRUE)
		close(f)
		continue
	}, list(path = path, value = value, continue = continue))

	# So.
	# There is no function to create the FIFO, but R will do that for us
	# if opening a connection for writing.
	# On POSIX, opening a FIFO with O_NONBLOCK|O_WRONLY will fail unless
	# there's already a reader, and O_NONBLOCK|O_RDWR is undefined;
	# opening without O_NONBLOCK will block until both ends are
	# connected.
	# On Windows, the fifo() implementation had significant bugs until
	# R-4.1.0: opening a non-existent fifo would crash R, and binary
	# mode was ignored.

	# (1) create a pipe but fail to open it
	try(suppressWarnings(close(fifo(path, 'w', FALSE))), TRUE)
	# (2) now that it exists, launch a child process
	Rscript(payload)
	# (3) now it's safe for both processes to open the FIFO
	f <- fifo(path, 'r', TRUE)
	on.exit(close(f), add = TRUE)
	unserialize(f)
}
