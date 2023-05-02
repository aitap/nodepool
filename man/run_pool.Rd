\name{run_pool}
\alias{run_pool}
\title{Run a pool server}
\description{
  This function starts a TCP server on a given port for the clients and
  the nodes to connect to. The server implements a pool that accepts
  tasks from the clients, distributes them between the nodes, and sends
  them back.
}
\usage{run_pool(port)}
\arguments{
  \item{port}{
    A whole number in \eqn{[1, 65535]} specifying the TCP port number to
    listen on.
  }
}
\details{
  Due to the way \code{\link{serverSocket}} currently works, the server
  is limited to IPv4 connections. This should not be a problem, because
  in absence of security measurements, the server has to be limited to
  local connections (possibly with SSH tunnels or VPNs) anyway.
}
\value{
  Does not return (enters an infinite loop) but can be safely
  interrupted.
}
%TODO: \references{}

\seealso{
  \code{\link{run_node}} for actually running the tasks%,
  %\code{\link{lbapply}} for the only client for now
}
\examples{\dontrun{run_pool(12345)}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
