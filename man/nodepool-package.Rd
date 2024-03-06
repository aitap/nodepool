\name{nodepool-package}
\alias{nodepool-package}
\alias{nodepool}
\docType{package}
\title{\packageTitle{nodepool}}
\description{\packageDescription{nodepool}}
\details{
  Run a pure-R compute pool. Tasks are dynamically distributed between
  the nodes that can disconnect and re-connect again.

  Use \code{\link{run_pool}(\var{port})} to start a pool on a given
  port. In order to send tasks to the pool, use
  \code{\link{pool_connect}(\var{host}, \var{port})} to create a
  connection object (make sure to \code{\link{close}} it when done),
  then run functions from package \pkg{parallel} to submit the tasks.
  In order to run the tasks, use \code{\link{run_node}(\var{host},
  \var{port})} in a new R process.

  \packageIndices{nodepool}
}
\author{
  Ivan Krylov

  Many thanks to Charlie Gao for starting the work on \PR{18587} and
  demonstrating that custom \pkg{parallel} back-ends are possible in the
  \CRANpkg{mirai} package.
}
% TODO:\references{}
\keyword{package}
\seealso{
  \code{\link{pool_connect}}, \code{\link{run_node}},
  \code{\link{run_pool}}
}
% TODO: real examples
\examples{
  \dontrun{
    # On one R process do:
    run_pool(12345)
  }
  \dontrun{
    # In another R process do:
    run_node('localhost', 12345)
  }
  \dontrun{
    # In a third R process do:
    cl <- pool_connect('localhost', 12345)
    parallel::parLapply(cl, 1:10, sin)
  }
}
