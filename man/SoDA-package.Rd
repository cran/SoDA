\name{SoDA-package}
\alias{SoDA-package}
\alias{SoDA}
\docType{package}
\title{
Functions and Other Code for the book "Software for Data Analysis"
}
\description{
This package contains R functions, some Fortran and C code, a little
miscellaneous code in other languages, and some data sets related to
the book "Software for Data Analysis".  Most of the functions
developed in the book will be found in this package, sometimes in a
more elaborated form.

In addition, there is a large collection of \R and other code and some
data sets, in the \code{"Examples"} directory of the package,
including the source used to generate a number of examples shown in the book.  The \R examples can
be used to reproduce the book's examples (not quite all, since a few
involve data that cannot be freely distributed).  

 The function
\code{\link{runExample}} can be used to run or access the material in
the  examples.  See its documentation for more details.  Examples can
be cited by file name or by the page on which the output of the
example begins.  See \code{\link{exampleFiles}} for how this works.

A related set of functions can be used to demonstrate \R examples a
line at a time, with the demonstrating human having the option to
insert typed expressions during the demonstration.  See
\code{\link{demoSource}}, including \code{demoExample} for the version
designed for using example files from the book.

In keeping with the subtitle of the book, some of the functions in
this package are slight extensions to programming facilities for \R;
for example, \code{\link{packageAdd}} and \code{\link{promptAll}} to
add software and documentation to a package.

}
\references{
Chambers, John M.(2008) \emph{Software for Data Analysis: Programming
  with R}, Springer.
}
\keyword{ package }
