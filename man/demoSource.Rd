\name{demoSource}
\alias{demoSource}
\alias{demoInput}
\alias{demoExample}
\title{Flexible execution of R source for demonstrations }
\description{
R expressions in a source file are shown and evaluated sequentially,
in an \R \dQuote{demo} session. Lines are alternately shown and
evaluated when the user
types empty lines to the function \code{demoInput} in another \R
session (the \dQuote{input} session) running in the same working
directory.  Any \R expression typed in the input session is shown and
evaluated in the demo, allowing you to add to the canned demo.
}
\usage{
demoSource(demo=, inputCon,  where = .GlobalEnv)
demoInput(path=)

demoExample(name, package = "SoDA")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{demo}{Either the file name or input connection for the R source to be demonstrated, or an object from class \code{"demoSource"}.  The latter allows resuming a demo.

By default, a menu interface prompts the user to select an R source
file in the local directory or type in a path string (see
\code{\link{localRFiles}} 
}
  \item{inputCon}{The connection from which to read prompt input.  Usually omitted in which case a call to \code{demoInput()} should occur in another R session in the same directory (see the details).}
  \item{where}{The environment where the demo expressions should be evaluated.  By default, in the global environment.}


  \item{path}{The file system path where user input is passed to the demo controller.  Must be a writable location and be known to both R sessions.  Usually omitted, in which case a suitable \code{fifo} is created to communicate with \code{demoSource()}.}


  \item{name}{For \code{demoExample()}, the name of the example file,
      with or without a \code{".R"} suffix.  There should be a
      corresponding file in the \code{"Examples"} subdirectory of the
      package.
      
      Once the file has been determined, \code{demoExample} just calls \code{demoSource}.
}
  \item{package}{The name of the package to use to find the example file.}
}
\details{
The demo is begun by starting \R twice in two separate shell
(terminal) windows, in the same working directory, the input window
and the demo window.
To start the demo, the user calls \code{demoInput} in the 
input window, with no arguments.
 The call to \code{demoInput} goes into a
loop reading terminal input.

In the demo window, the user then calls \code{demoSource}, usually just supplying the file name for the source file.
The input process now reads lines from the terminal and writes to a
\code{\link{fifo}} that will be read by the demo process.  Empty lines (the usual) alternately display and evaluate single lines from the source file.
A line consisting only of a comma is a continuation:  The next line of the source is read and displayed but not yet evaluated.  Use this mechanism to collect a bunch of lines to be evaluated together, such as a function definition.  When the last line of the bunch is displayed, enter an empty line to evaluate all the lines at once.

A line containing only \code{q} quits from \code{demoInput}, which you
need to do before starting to run another demo.
Quitting from the input also causes the quit command to be written to
the fifo, at which point \code{demoSource} returns the current state
of the demo.
If you quit before the demo is finished, \emph{and} you have arranged
to assign the returned value from \code{demoSource}, that object can
be supplied in a subsequent call to \code{demoSource} to resume this
demo.

Any input other than an empty line, comma or \code{q} is interpreted as a literal expression that the user wants evaluated instead of the next source line.

Source lines in the demo file ending in \code{"#SILENT"} will be silently executed before the next ordinary line is displayed.
}
\value{
  \code{demoSource()} and \code{demoExample()} return, invisibly, the \code{"demoSource"} object describing the current state of the demo.
}
\seealso{
	\code{\link{demoSource-class}()}
}
\keyword{programming}
