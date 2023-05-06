\name{run_pool}
\alias{run_pool}
\alias{run_pool_background}
\alias{print.run_pool_background}
\title{Run a pool server}
\description{
  This function starts a TCP server on a given port for the clients and
  the nodes to connect to. The server implements a pool that accepts
  tasks from the clients, distributes them between the nodes, and sends
  them back.
}
\usage{
  run_pool(port)
  run_pool_background(port = NULL)
  \method{print}{run_pool_background}(x, \dots)
}
\arguments{
  \item{port}{
    A whole number in \eqn{[1, 65535]} specifying the TCP port number to
    listen on. May be omitted for \code{run_pool_background}, in which
    case a free port will be chosen automatically.
  }
  \item{x}{
    An object returned by \code{run_pool_background}.
  }
  \item{\dots}{
    Extra arguments to \code{print.run_pool_background} are not
    allowed.
  }
}
\details{
  Due to the way \code{\link{serverSocket}} currently works, the server
  is limited to IPv4 connections. This should not be a problem, because
  in absence of security measurements, the server has to be limited to
  local connections (possibly with SSH tunnels or VPNs) anyway.

  Once the pool is started as a background process, no channel of
  control is retained. In particular, the process will \emph{not} be
  stopped automatically when the returned object becomes subject to
  garbage collection or the parent R session terminates. The process
  \emph{can} be stopped by connecting to the pool and using
  \code{\link{pool_halt}()}.
}
\value{
  \item{\code{run_pool}}{
    Does not return (enters an infinite loop) but can be safely
    interrupted.
  }
  \item{\code{run_pool_background}}{
    An integer scalar of class \code{run_pool_background}, signifying
    the port number (whether it was chosen automatically or not).
    Additionally, the PID of the \code{Rscript} process is stored in the
    \code{pid} attribute, but only for informational purposes.
  }
}
%TODO: \references{}

\seealso{
  \code{\link{run_node}} for actually running the tasks,
  \code{\link{lbapply}} for the only client for now
}
\examples{\dontrun{run_pool(12345)}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
