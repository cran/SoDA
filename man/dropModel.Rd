\name{dropModel}
\alias{dropFormula}
\alias{dropModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Drop terms from a model }
\description{
  Either updates a model or modifies the formula to drop all terms involving a specified variable.
}
\usage{

dropFormula(original, drop)

dropModel(model, drop)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{original}{ Original formula }
  \item{drop}{Which variable to drop }

  \item{model}{Original fitted model}
}

\value{
 The modified model object or formula.
}

\keyword{programming}
