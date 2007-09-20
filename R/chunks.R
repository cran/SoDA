## R functions to interface to Perl routines
## The Perl code routines have been defined when SoDA was loaded

chunksAdd <- function( table = .PerlExpr("\\%{0};", .convert = FALSE),
                      data = character(),
                      convert = length(data) == 0) {
    if(!inherits(table, "PerlHashReference"))
      stop(gettextf(
       "Argument table must be reference to a Perl hash object; got an object of class \"%s\"",
                    class(table)), domain = NA)
    args <- c(list(table), as.list(data))
   .Perl("chunks_add",  .args = args, convert = convert)
}

chunksDrop <- function(table,
                       data = character(),
                       convert = length(data) == 0) {
    if(missing(table))
      stop("Must start with a non-empty table")
    else if(!inherits(table, "PerlHashReference"))
      stop(gettextf(
       "Argument table must be reference to a Perl hash object; got an object of class \"%s\"",
                    class(table)), domain = NA)
    args <- c(list(table), as.list(data))
   .Perl("chunks_drop",  .args = args, convert = convert)
}
