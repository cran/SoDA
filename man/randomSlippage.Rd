\name{randomSlippage}
\alias{randomSlippage}
\title{ Are simulated sequences robust? }
\description{
  This function inserts a specified number of extra random uniforms into a sequence of calls to a generator, and checks whether the subsequent samples re-synchronize.
}
\usage{
randomSlippage(nRuns, expr1, expr2, slip = runif(1), check = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nRuns}{Number of runs for the comparison. }
  \item{expr1, expr2}{ The literal expressions to be evaluated before and after the possible slippage. }
  \item{slip}{ The expression to be evaluated to cause the slippage; default \code{runif(1)}. }
    \item{check}{If \code{TRUE}, the function will check that synchronization really did occur.}
}
\details{
  The second generated result from evaluatging \code{expr2} will resynchronize, if ever, after some number of values generated in the original and perturbed sequence, say \code{k1} and \code{k2}.  At this point, each sequence returns exactly the same value because each has used the same number of uniforms; from that point on the sequences will be identical.

Re-synchronization need never occur; see the example in the reference.
}
\value{
  The function returns a matrix with \code{nRuns} rows and two columns.  For each row, the returned value is the slippages, \code{c(k1, k2)} in the Details.
}
\examples{
set.seed(211)
RNGkind("default", "Ahrens")
xx = randomSlippage(1000, rnorm(20), rnorm(20))
table(xx[,1], xx[,2])
}
\keyword{distribution}
\keyword{programming}
