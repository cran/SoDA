
.hasRSPerl <- FALSE

.onLoad <- function(libname, pkgname) { 
   if(tryRequire("RSPerl")) {
       .hasRSPerl <<- TRUE
       perlFiles <- readLines(system.file("Perl/perlFiles.txt", package = "SoDA"))
       for(file in perlFiles) {
           ex <- try(.PerlFile(system.file("Perl", file, package = "SoDA")))
           if(inherits(ex, "try-error"))
             warning("error in running Perl file ", file, "; some Perl subroutines may be missing")
       }
   }
}
