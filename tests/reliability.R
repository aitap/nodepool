library(nodepool)
library(parallel)

pool <- run_pool(background = TRUE, nodes = 2)

nodepids <- unlist(attr(pool, 'nodepids'))
print(results <- parLapply(pool, 2:3, function(x, nodepids) {
	pid <- Sys.getpid()
	if (pid == nodepids[1]) q('no') # simulate a crash
	c(pid, x^2)
}, nodepids = nodepids))
stopifnot(
	# one node must survive
	vapply(results, `[[`, 0, 1) == nodepids[2],
	# both tasks must be complete
	vapply(results, `[[`, 0, 2) == (2:3)^2
)

newnode <- run_node('localhost', attr(pool, 'port'), TRUE)
eff.pids <- unlist(parLapply(pool, 1:2, function(.) {
	Sys.sleep(.5); Sys.getpid()
}))
stopifnot(
	# new node must be accepted
	setequal(eff.pids, c(nodepids[2], newnode))
)
tools::assertError(
	print(parLapply(pool, 1, function(.) stop("This must fail")))
)

stopCluster(pool)
