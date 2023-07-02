# Start an Rscript background process in the background and have it run
# a given expression. Cannot even return the PID, though it's mostly
# useless without a guarantee that it belongs to the same process.
Rscript <- function(expr) system2(
	file.path(R.home('bin'), 'Rscript'),
	# can't use input = text here because Rscript isn't guaranteed to be
	# fast enough to read the temp file before it's deleted by system2()
	c('-e', shQuote(paste(deparse(expr), collapse = '\n'))),
	wait = FALSE
)

# Start a background Rscript but have it return a value to the parent
# process, blocking until it's returned. Continue executing a different
# expression.
Rscript_payload <- function(value, continue) {
	# How to receive a piece of information from a background Rscript
	# and have it continue? Well...
	# We can't use pipe() (which is implemented using popen()) because
	# pclose() will wait until the child process terminates and we don't
	# want that. (Keeping the connection open is not worth the hassle.)
	# We could use fifo(), but it had significant bugs on Windows until
	# R-4.1.0.
	# Hence... temporary files as IPC!

	path <- tempfile('Rscript_payload_rds')
	on.exit(unlink(path), add = TRUE)

	Rscript(substitute({
		t <- tempfile(tmpdir = dirname(path))
		saveRDS(value, t)
		file.rename(t, path)
		continue
	}, list(path = path, value = value, continue = continue)))

	repeat {
		ret <- try(suppressWarnings(readRDS(path)), TRUE)
		if (!inherits(ret, 'try-error')) break
		Sys.sleep(.5)
	}
	ret
}
