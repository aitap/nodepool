nodepool
========

What is this?
-------------

`nodepool` is an [R] package that implements a computational pool
similar to R's built-in `parallel` clusters, except that it can
dynamically grow and shrink.

The pool server is a TCP server to which clients and nodes can connect. Clients
send computational tasks to the pool server for the nodes to evaluate; the
nodes send the results and the pool server forwards them back to the clients.

![Top to bottom, there are boxes labelled "client", a single box
labelled "pool server" and more boxes labelled "node". Arrows labelled "tasks"
go from the "clients" to the "pool server", then from the "pool" to the
"nodes".](man/figures/architecture.svg)

Installation
------------

You can install the source package from the private repo:

```
install.packages('nodepool', contriburl='https://aitap.github.io/nodepool-releases', type = 'source')
```

Alternatively, you can install `nodepool` from GitHub using the `remotes`
package.

```
remotes::install_github("aitap/nodepool")
```

How can I use it?
-----------------

Pick the machine that is visible from all other machines and note its
network address. This will be the pool server. In this example, the pool
server has an address of `192.0.2.1`. Pick an unoccupied [port number]
too; we'll be using `12345` here.

Mind that there's currently no security, so don't start this on a
publicly available connection; instead use [SSH tunnels]. Every node and
client will need to make outbound connections to the pool server, but
there is no need for clients or nodes to be able to talk to each other;
neither is there any need for the pool server to be able to connect
back to the clients or the nodes.

Start an `R` process on the pool server, install `nodepool` and run:

```
nodepool::run_pool(12345)
```

On every machine that's intended to be running the tasks (nodes), start an `R`
process, install `nodepool` and run:

```
nodepool::run_node('192.0.2.1', 12345)
```

Pass the `nodes` argument to `run_pool()` in order to have the pool
automatically include the specified number of nodes running on the same
machine as the server.

On the client machine(s), load the package using `library(nodepool)`.
Use `pool <- pool_connect('192.0.2.1', 12345)` in order to connect to
the pool.  Currently, there's only one function that submits tasks to
the pool, and it's `lbapply(list, function, pool, ...)`. It is more or
less similar to `parallel::parLapplyLB`.

If you'd like to run a pool server in a background process without
dedicating the current R session for that pass the `background = TRUE`
argument to `run_node()`. In this case, the return value is a pool
connection like the one returned by `pool_connect()`.  Use
`pool_halt(pool)` to stop the background process once you're done.

Nodes can also be started in the background, using the same
`background = TRUE` argument to `run_node()`. The background node
process will when the pool is halted, or you'll can interrupt it
manually using the facilities provided by your operating system.

If a node's connection drops, its tasks are redistributed to other
nodes, but it's welcome to connect again later. Additional nodes can
join the pool at any time as well.

Anything I should keep in mind?
-------------------------------

`nodepool` is still at an early stage of development.

There are no security measures implemented in `nodepool`. Just like in
`parallel` clusters, connections are unencrypted and go straight to R's
`unserialize()`. Don't put it on public networks.

Unlike in `parallel` clusters, there's no reproducibility guarantee.
Since we're using load balancing, the tasks get assigned to nodes in an
unpredictable order, so even with `set.seed()` results can be different
between runs due to differences between floating-point units in the CPUs
used by the nodes.

There is currently no protection against a client disconnecting from
the pool. When this happens, all its tasks are removed from the queue,
as well as unsent results. Neither does `lbapply` keep parts of
unfinished jobs when an exception is raised for one reason or another.
There's an idea to implement some form of [caching] on both the client
(don't send the tasks if they're already complete) and the server
(remember the results for previously submitted tasks), but [caches are
hard] to implement properly and may interfere with computational
experiments involving random numbers. Tasks could in theory include
random seeds (and nodes may restore the seed afterwards), but it doesn't
feel right as a solution.

The package is currently implemented in pure R and makes use of R's
connection system. Depending on your build of R, you may be limited to
128 connections in total, including at least three for
`stdin`/`stdout`/`stderr`. The good news is that only the pool server
needs a custom build of R with increased connection limits: every client
and every node only needs one connection to access the pool. The package
may later get rewritten in [ZeroMQ] in order to both sidestep this limit
and provide a measure of protection (the server and the clients mutually
authenticating each other using asymmetric cryptography in the CURVE
extension).

[R]: https://www.r-project.org/
[port number]: https://en.wikipedia.org/wiki/Transmission_Control_Protocol#TCP_ports
[SSH tunnels]: https://en.wikipedia.org/wiki/SSH_tunnel#Secure_Shell_tunneling
[caching]: https://cran.r-project.org/package=depcache
[caches are hard]: https://devblogs.microsoft.com/oldnewthing/20060502-07/?p=31333
[ZeroMQ]: https://zeromq.org/
