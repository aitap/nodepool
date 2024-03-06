\name{pool_connect}
\alias{pool_connect}
\alias{print.nodepool_cluster}
\alias{close.nodepool_cluster}
\alias{stopCluster.nodepool_cluster}
\title{Connections to a pool}
\description{
  Establish a TCP connection to a pool that was started using
  \code{\link{run_pool}} or stop the pool.
}
\usage{
  pool_connect(host, port, length = 128)
  \method{print}{nodepool_cluster}(x, \dots)
  \method{close}{nodepool_cluster}(con, \dots)
  \method{stopCluster}{nodepool_cluster}(cl, \dots)
}
\arguments{
  \item{host}{
    The IPv4 address (or a hostname) to connect to as a single string.
  }
  \item{port}{
    The TCP port number to connect to, a whole number in
    \eqn{[1, 65535]}.
  }
  \item{x, con, cl}{
    A \code{nodepool_cluster} object returned by \code{pool_connect}.
  }
  \item{length}{
    The number of virtual nodes in the \R-level cluster. Limits the
    number of tasks evaluated at the same time. There is probably no
    benefit to setting it higher than the maximal total number of nodes
    that can connect to the pool.
  }
  \item{\dots}{
    Ignored.
  }
}
\details{
  The connection is set up to be blocking and binary in order to let
  \code{\link{serialize}} traffic through. The values of \code{host} and
  \code{port} are remembered, but not yet used anywhere.

  The \code{stopCluster} meethod sends a termination message to the pool
  and then closes the connection. If you'd like to close the connection
  without stopping the pool, use \code{\link{close}}.
}
\value{
  \code{pool_connect}: a \code{\link{connection}} object of class
  \code{pool_connection}. The attributes \code{host} and \code{port}
  store the connection information.
}
\note{
  This function will probably be rewritten or renamed when a better idea
  for the API shows up. At the very least, the connection object will
  have to become mutable in order to re-establish the connection
  automatically.
}
\examples{\dontrun{
  pool <- pool_connect('192.2.0.2', 12345)
  print(pool)
  close(pool)
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
