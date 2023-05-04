\name{lbapply}
\alias{lbapply}
\title{Apply a Function over a List Using a Pool}
\description{
  Distribute the function and the elements of a given list as tasks to a
  computational pool and collect the results into a list.
}
\usage{lbapply(x, fun, pool, ...)}
\arguments{
  \item{x}{
    A vector (atomic or a list) consisting of elements to apply
    \code{fun} to.
  }
  \item{fun}{
    The function to apply to each element of \code{x}.
  }
  \item{pool}{
    A connection object returned by \code{\link{pool_connect}}.
  }
  \item{\dots}{
    Forwarded to \code{fun(x[[i]], ...)}.
  }
}
\details{
  The pool distributes the tasks to the nodes dynamically, so there's no
  reproducibility guarantee when it comes to random numbers or floating
  point rounding errors. If all the nodes in the pool use the same
  hardware and software, it may be possible to have reproducible results
  by preparing a vector of random seeds and making it part of the task:

  \preformatted{
    seeds <- floor(runif(N_tasks, 0, .Machine$integer.max+1))
    # create a list of tasks...
    lbapply(tasks, function(task) {
      set.seed(task$seed)
      # continue...
    }, pool)
  }
}
\value{
  A list consisting of return values of \code{fun} applied to each
  element of \code{x}. No simplification is currently performed.
}
\note{
  This function should probably be taught to re-connect to the pool when
  the connection is lost and to cache the computations in case the
  client computer crashes. The latter is particularly hard to do right.
}

\seealso{
  \code{\link{pool_connect}}
}
% FIXME: perhaps we should merge ?lbapply and ?pool_connect (or their
% API-redesigned versions)
\examples{\dontrun{
  pool <- pool_connect('192.2.0.1', 12345)
  lbapply(1:10, sin, pool)
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
