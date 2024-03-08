\name{run_pool}
\alias{run_pool}
\title{Run a pool server}
\description{
  This function starts a TCP server on a given port for the clients and
  the nodes to connect to. The server implements a pool that accepts
  tasks from the clients, distributes them between the nodes, and sends
  them back.
}
\usage{
  run_pool(
    port = NULL, background = FALSE, nodes = 0,
    verbose = TRUE, \dots
  )
}
\arguments{
  \item{port}{
    A whole number in \eqn{[1, 65535]} specifying the TCP port number to
    listen on. May be omitted when starting a background server, in
    which case a free port will be chosen automatically.
  }
  \item{background}{
    By default, the pool is started in the current R session. Set this
    argument to \code{TRUE} to run the pool in a background process
    instead.
  }
  \item{nodes}{
    The number of nodes to automatically launch as background processes
    to be included in the pool.
  }
  \item{verbose}{
    Whether to provide a log of what is happening in the pool to the
    standard output of the process running the pool.
  }
  \item{\dots}{
    When starting the pool in the background, forwarded to
    \code{\link{pool_connect}}. Otherwise must be empty.
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
  \code{\link[parallel]{stopCluster}()}.
}
\value{
  When \code{background = FALSE}: does not return (enters an infinite
  loop) but can be safely interrupted.

  Otherwise, a \code{pool_connection} object, the same as returned by
  \code{\link{pool_connect}}. Additionally, the PID of the
  \code{Rscript} process is stored in the \code{pid} attribute and the
  PIDs of the automatically started nodes are stored in the
  \code{nodepids} attribute, but only for informational purposes.
}
%TODO: \references{}

\seealso{
  \code{\link{run_node}} for actually running the tasks.
}
\examples{\dontrun{run_pool(12345)}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
