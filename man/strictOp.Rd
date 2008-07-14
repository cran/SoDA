\name{strictOp}
\alias{strictOp}
\alias{withStrictOps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Apply strict conditions to R operators }
\description{
  The expression is checked for stricter conditions on compatible arguments; if the conditions fail an error or warning is issued. If no error is issued, the expression's value is returned.
}
\usage{
withStrictOps(expr, attach)
strictOp(expr, warnOnly = FALSE, errorCall)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expr}{ For \code{withStrictOps}, any expression to be evaluated. Subexpressions from the  binary operators in the base package for arithmetic, comparison, and logic will be evaluated by stricter rules.  For \code{strictOp}, the expression should only be one of these calls.  Generally, \code{withStrictOp} will be more convenient to use, and \code{strictOp} is largely for internal use. }
    \item{attach}{No longer supported in CRAN, because of restriction
        added long after this package was written.  Used to be:

Optional argument, only examined if \code{expr} is missing.  If \code{TRUE}, then the \code{"strictOps"} environment is attached, overriding the applicable operators with strict versions.  If \code{attach} is \code{FALSE}, this environment is detached. See the details.}
  \item{warnOnly}{If \code{TRUE}, only issue warnings on failures.}
    \item{errorCall}{Optional argument for internal use, supplying the expression to be used in error messages as the original call.}
}
\details{
  The functions apply the stricter rules for compatibility given in section 7.1 of \dQuote{Software for Data Analysis}.

Calling \code{withStrictOps} evaluates an arbitrary expression with an environment containing strict versions of all the relevant operators.
Calling \code{withStrictOps} with no arguments attaches this environment to the search list, overriding the base versions of the operators.  If the expressions pass the strict rules, evaluation is passed on to the corresponding base package version of the operator.
}
\value{
  the value of \code{expr}
}
\examples{
sum(rnorm(3) ==  as.raw(1:3))# succeeds but comparison is ambiguous
muststop(withStrictOps(sum(rnorm(3) ==  as.raw(1:3))),
          silent = FALSE)  # signals an error

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{programming}
