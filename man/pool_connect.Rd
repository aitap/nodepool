\name{pool_connect}
\alias{pool_connect}
\title{Connect to a pool}
\description{
  Establish a TCP connection to a pool that was started using
  \code{\link{run_pool}}.
}
\usage{pool_connect(host, port)}
\arguments{
  \item{host}{
    The IPv4 address (or a hostname) to connect to as a single string.
  }
  \item{port}{
    The TCP port number to connect to, a whole number in
    \eqn{[1, 65535]}.
  }
}
\details{
  The connection is set up to be blocking and binary in order to let
  \code{\link{serialize}} traffic through.
}
\value{
  A \code{\link{connection}} object opened in the right mode
  corresponding to the TCP connection.
}
\note{
  This function will probably be rewritten or renamed when a better idea
  for the API shows up. At the very least, the host and the port need to
  be remembered in order to re-establish the connection automatically.
}

\seealso{\code{\link{lbapply}}}
\examples{\dontrun{pool <- pool_connect('192.2.0.2', 12345)}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
