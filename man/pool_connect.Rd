\name{pool_connect}
\alias{pool_connect}
\alias{pool_halt}
\alias{print.pool_connection}
\title{Connections to a pool}
\description{
  Establish a TCP connection to a pool that was started using
  \code{\link{run_pool}} or stop the pool.
}
\usage{
  pool_connect(host, port)
  pool_halt(pool)
  \method{print}{pool_connection}(x, \dots)
}
\arguments{
  \item{host}{
    The IPv4 address (or a hostname) to connect to as a single string.
  }
  \item{port}{
    The TCP port number to connect to, a whole number in
    \eqn{[1, 65535]}.
  }
  \item{pool, x}{
    A \code{pool_connection} object returned by \code{pool_connect}.
  }
  \item{\dots}{
    No other arguments to \code{print.pool_connection} are allowed.
  }
}
\details{
  The connection is set up to be blocking and binary in order to let
  \code{\link{serialize}} traffic through. The values of \code{host} and
  \code{port} are remembered, but not yet used anywhere.

  The \code{pool_halt} function sends a termination message to the pool
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

\seealso{\code{\link{lbapply}}}
\examples{\dontrun{
  pool <- pool_connect('192.2.0.2', 12345)
  close(pool)
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
