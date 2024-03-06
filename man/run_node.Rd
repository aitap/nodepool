\name{run_node}
\alias{run_node}
\alias{print.run_node}
\title{Run a computational node in a pool}
\description{
  This function connects to a pool at the given host and port, sets up
  a temporary environment and evaluates the incoming tasks in a loop.
}
\usage{
  run_node(host, port, background = FALSE)
  \method{print}{run_node}(x, \dots)
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
  \item{background}{
    By default, the node is started in the current process (and keeps
    working in an infinite loop). Set this parameter to \code{TRUE} to
    start the node in a background process.
  }
  \item{x}{
    An object returned by \code{run_node}.
  }
  \item{\dots}{
    No other arguments are allowed.
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

  If starting a background process, no means of controlling it are
  retained. Please don't rely on the PID returned by the function in
  this case, because it may be reused after the process terminates for
  unrelated reasons.
}
\value{
  If \code{background = FALSE}: does not return (enters an infinite
  loop) but can be safely interrupted.

  If \code{background = TRUE}: returns an object of class
  \code{run_node} containing the PID of the background process when it
  started.
}
%TODO: \references{}

\seealso{
  \code{\link{run_pool}}
}
\examples{\dontrun{run_node('localhost', 12345)}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
