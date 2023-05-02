\name{run_node}
\alias{run_node}
\title{Run a computational node in a pool}
\description{
  This function connects to a pool at the given host and port, sets up
  a temporary environment and evaluates the incoming tasks in a loop.
}
\usage{
  run_node(host, port)
}
\arguments{
  \item{host}{
    The host to connect to. Can be a hostname or an IP address.
    Forwarded to \code{\link{socketConnection}}.
  }
  \item{port}{
    The TCP port to connect to. Should be a whole number in
    \eqn{[1, 65535]}. Forwarded to \code{\link{socketConnection}}.
  }
}
\details{
  Before accepting any tasks from the pool, this function creates two
  temporary directories. One of them is set up as the \link{library} for
  the duration of the call; the other becomes the working directory.
  This makes it relatively safe to submit tasks that involve
  \code{\link{install.packages}} or create files in the current
  directory. On exit, both the current directory and the library paths
  are restored.

  The tasks are evaluated in the \code{base} package namespace, so any
  required packages should be attached explicitly.
}
\value{
  Does not return (enters an infinite loop) but can be safely
  interrupted.
}
%TODO: \references{}

\seealso{
  \code{\link{run_pool}}%, \code{\link{lbapply}}
}
\examples{\dontrun{run_node('localhost', 12345)}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
