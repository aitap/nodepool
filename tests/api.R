library(nodepool)
library(parallel)

# NOTE: staticClusterApply will send a job containing a call to lapply()
# to every node in the cluster, some of them with 0-length lists as
# arguments. We have to ensure length(cluster) == n_of_nodes, otherwise
# there is a significant chance that one of the nodes only gets empty
# lists as tasks and so doesn't show up.
pool <- run_pool(background = TRUE, nodes = 2, length = 2)

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
print(eff.pids <- unlist(parLapply(pool, 1:2, function(.) {
	Sys.sleep(.5); Sys.getpid()
})))
stopifnot(
	# new node must be accepted
	setequal(eff.pids, c(nodepids[2], newnode))
)
tools::assertError(
	print(parLapply(pool, 1, function(.) stop("This must fail")))
)
stopCluster(pool)

# Must successfully process a parLapply() of more entries than nodes in
# the pool
cl <- run_pool(background = TRUE, nodes = 2, length = 32)
stopifnot(all.equal(
	as.list(letters),
	parLapply(cl, LETTERS, function(x) {
		Sys.sleep(.05)
		tolower(x)
	})
))
stopCluster(cl)
