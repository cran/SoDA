
.requireSilent <- function(what) {
    tryCatch(require(what, quietly = TRUE, character.only = TRUE),
             warning = function(cond) FALSE,
             error = function(cond) FALSE)
}

.hasRSPerl <- FALSE

.onLoad <- function(libname, pkgname) { 
   if(.requireSilent("RSPerl")) {
       .hasRSPerl <<- TRUE
       perlFiles <- readLines(system.file("Perl/perlFiles.txt", package = "SoDA"))
       for(file in perlFiles) {
           ex <- try(.PerlFile(system.file("Perl", file, package = "SoDA")))
           if(is(ex, "try-error"))
             warning("error in running Perl file ", file, "; some Perl subroutines may be missing")
       }
   }
}
