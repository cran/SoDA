% $Id: digest.Rd,v 1.1 2007/05/22 14:25:42 jmc Exp $
\name{digest}
\alias{digest}
\title{Create hash function digests for arbitrary R objects}
\description{
  The \code{digest} function applies a cryptographical hash function to
  arbitrary \R objects. By default, the objects are internally
  serialized, and either one of the currently implemented MD5 and SHA-1
  hash functions algorithms can be used to compute a compact digest of
  the serialized object.

  This version of the function accomplishes essentially the same
  result as the function of the same name in the \code{digest}
  package, but via somewhat different computations, as discussed in
  the book \dQuote{Software for Data Analysis}.
}
\usage{
digest(object, algo=c("md5", "sha1", "crc32"), 
       serialize=TRUE, file=FALSE, length=Inf, use.Call = FALSE)
}
\arguments{
  \item{object}{An arbitrary R object which will then be passed to the
    \code{\link{serialize}} function, unless the \code{serialize}
    argument is set to \code{FALSE}}
  \item{algo}{The algorithms to be used; currently available choices are
    \code{md5}, which is also the default, \code{sha1} and \code{crc32}}
  \item{serialize}{A logical variable indicating whether the object
    should be serialized using \code{serialize}. Setting this to
    \code{FALSE} allows to compare the digest output of given character
    strings to known control output.}
  \item{file}{A logical variable indicating whether the object is a file name.}
  \item{length}{Number of characters to process. By default, when
    \code{length} is set to \code{Inf}, the whole string or file is
    processed.}
  \item{use.Call}{Should the C interface use the \code{.Call()}
    interface or the \code{.C()} interface.  An internal question
    whose answer should not affect the result.}
}
\value{
  The \code{digest} function returns a character string of a fixed
  length containing the requested digest of the supplied R object. For
  MD5, a string of length 32 is returned; for SHA-1, a string of length
  40 is returned; for CRC32 a string of length 8.
}
\details{
  See the documentation for the \code{digest} package version of the
  function, and the references below for the underlying algorithms.

 The version in the present package has been modified for tutorial
 reasons, to illustrate some principles of the design of interfaces to
 C code.
}
\references{
  MD5: \url{https://www.ietf.org/rfc/rfc1321.txt}. 

  SHA-1: \url{https://www.itl.nist.gov/fipspubs/fip180-1.htm}.

  CRC32:  \url{https://zlib.net/crc_v3.txt}.

  The page for the code underlying the C functions used here for sha-1
  and md5, and further references, is no longer accessible.  Please see
  \url{https://en.wikipedia.org/wiki/SHA-1} and
  \url{https://en.wikipedia.org/wiki/MD5}.

  \url{http://zlib.net} for documentation on the zlib library which
  supplied the code for crc32.
}
\author{Dirk Eddelbuettel \email{edd@debian.org} for the original \R interface;
  Antoine Lucas for the integration of crc32; Jarek Tuszynski for the
  file-based operationss; Christophe Devine for the hash function
  implementations for sha-1 and md5; Jean-loup Gailly and Mark Adler
  for crc32.

John Chambers for the modified C and \R code in this package.} 
\seealso{\code{\link{serialize}}, \code{\link{md5sum}}}

\examples{

## Standard RFC 1321 test vectors
md5Input <-
  c("",
    "a",
    "abc",
    "message digest",
    "abcdefghijklmnopqrstuvwxyz",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
    paste("12345678901234567890123456789012345678901234567890123456789012",
          "345678901234567890", sep=""))
md5Output <-
  c("d41d8cd98f00b204e9800998ecf8427e",
    "0cc175b9c0f1b6a831c399e269772661",
    "900150983cd24fb0d6963f7d28e17f72",
    "f96b697d7cb7938d525a2f31aaf161d0",
    "c3fcd3d76192e4007dfb496cca67e13b",
    "d174ab98d277d9f5a5611c2c9f419d9f",
    "57edf4a22be3c955ac49da2e2107b67a")

for (i in seq(along=md5Input)) {
  md5 <- digest(md5Input[i], serialize=FALSE)
  stopifnot(identical(md5, md5Output[i]))
}

sha1Input <-
  c("abc",
    "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
    NULL)
sha1Output <- 
  c("a9993e364706816aba3e25717850c26c9cd0d89d",
    "84983e441c3bd26ebaae4aa1f95129e5e54670f1",
    "34aa973cd4c4daa4f61eeb2bdbad27316534016f")

for (i in seq(along=sha1Input)) {
  sha1 <- digest(sha1Input[i], algo="sha1", serialize=FALSE)
  stopifnot(identical(sha1, sha1Output[i]))
}

crc32Input <-
  c("abc",
    "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
    NULL)
crc32Output <- 
  c("352441c2",
    "171a3f5f",
    "2ef80172")

for (i in seq(along=crc32Input)) {
  crc32 <- digest(crc32Input[i], algo="crc32", serialize=FALSE)
  stopifnot(identical(crc32, crc32Output[i]))
}

# one of the FIPS-
sha1 <- digest("abc", algo="sha1", serialize=FALSE)
stopifnot(identical(sha1, "a9993e364706816aba3e25717850c26c9cd0d89d"))

# example of a digest of a standard R list structure
digest(list(LETTERS, data.frame(a=letters[1:5], b=matrix(1:10,ncol=2))))

# test 'length' parameter and file input
fname = file.path(R.home(),"COPYING")
x = readChar(fname, file.info(fname)$size) # read file
for (alg in c("sha1", "md5", "crc32")) {
  # partial file
  h1 = digest(x    , length=18000, algo=alg, serialize=FALSE)
  h2 = digest(fname, length=18000, algo=alg, serialize=FALSE, file=TRUE)
  h3 = digest( substr(x,1,18000) , algo=alg, serialize=FALSE)
  stopifnot( identical(h1,h2), identical(h1,h3) )
  # whole file
  h1 = digest(x    , algo=alg, serialize=FALSE)
  h2 = digest(fname, algo=alg, serialize=FALSE, file=TRUE)
  stopifnot( identical(h1,h2) )
}

# compare md5 algorithm to other tools
library(tools)
fname = file.path(R.home(),"COPYING")
h1 = as.character(md5sum(fname))
h2 = digest(fname, algo="md5", file=TRUE)
stopifnot( identical(h1,h2) )
}
\keyword{misc}

