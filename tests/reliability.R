library(nodepool)

# need a local environment for on.exit
local({
	pool <- run_pool(background = TRUE, nodes = 2)
	on.exit(pool_halt(pool))

	nodepids <- unlist(attr(pool, 'nodepids'))
	results <- lbapply(2:3, function(x) {
		pid <- Sys.getpid()
		if (pid == nodepids[1]) q('no') # simulate a crash
		c(pid, x^2)
	}, pool)
	stopifnot(
		# one node must survive
		vapply(results, `[[`, 0, 1) == nodepids[2],
		# both tasks must be complete
		vapply(results, `[[`, 0, 2) == (2:3)^2
	)

	newnode <- run_node('localhost', attr(pool, 'port'), TRUE)
	eff.pids <- unlist(lbapply(1:2, function(.) {
		Sys.sleep(.5); Sys.getpid()
	}, pool))
	stopifnot(
		# new node must be accepted
		setequal(eff.pids, c(nodepids[2], newnode))
	)

	# let on.exit clean up the pool
})
