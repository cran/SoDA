
demoSource <- function(demo, inputCon = .demoFifo(),  where = .GlobalEnv) {
    if(!isOpen(inputCon))
      stop("The connection for control input is not open:  usually, you should call demoInput() in a separate R session.")
    on.exit(if(!identical(inputCon, stdin()))
       close(inputCon))
    if(!is(demo, "demoSource"))
      demo <- .sourceAsDemo(demo)
    demo@envir <- as.environment(where)
    while(.moreSource(demo)) {
        .showPrompt(demo@state)
         control <- .getNextLine(inputCon)
        if(identical(control, "")) { # empty, alternate display & eval
            demo <- .doParseEval(demo)
        }
        else {
            key <- match(control, .keyStrings)
            if(is.na(key)) {
                demo@partial <- c(demo@partial, control)
                cat(control,"\n", sep="", file = stderr())
                demo <- .doParseEval(demo, eval = TRUE, read = FALSE)
            }
            else 
            switch(names(.keyStrings)[[key]],
                   complete = {
                       demo <- .doParseEval(demo, eval = TRUE)
                   },
                  continue = {
                      demo <- .nextSourceLine(demo)
                  },
                   quit = {
                       demo@state <- "quit"
                       break
                       },
                   message("Oops, got a key (\"", key, "\"), but there's no corresponding action--looks like a bug in demoSource()")
                   )
            
        }
    }
    ## process last expression, if any
    switch(demo@state,
           parsed = 
               demo <-  .doParseEval(demo),
           partial =
               warning("demo source ended with a partial expression: ", paste(demo@partial, collapse = "\n")))
    invisible(demo)
}
demoExample <- function(name, package = "SoDA") {
    file <- exampleFiles(name, package)
    if(length(file) == 0)
      stop("No example file found for \"",name, "\"")
    demoSource(file)
}

setClass("demoSource",
       representation(lines = "character",
                      pos = "numeric",
                      partial = "character",
                      expr = "expression",
                      value = "ANY",
                      envir = "environment",
                      state = "character"),
         prototype = prototype(pos = 0, state = "initial")
         )

exampleFiles <- function(names = character(), package = "SoDA") {
    .matchFile <- function(thisName, files) {
        i <- match(thisName, files)
        if(is.na(i)) {
            i <- match(paste(thisName, ".R", sep=""), files)
            if(is.na(i)) {
                candidates <- paste(thisName, c(".S", ".q", ".r", ".s"), sep = "")
                i <- match(candidates, files)
                i <- i[!is.na(i)]
                if(length(i) > 1){
                    i <- i[[1]]
                    warning("ambiguous match, using \"", candidates[[i]], "\"")
                }
                else if(length(i) == 0)
                  i <- NA
            }
        }
        if(is.na(i))
          character()
        else
          files[i]
    }
    files <- list.files(system.file("Examples", package = package))
    if(length(names) == 0 || identical(names, ""))
      files
    else {
        found <- character()
        for(thisName in names)
          found <- c(found, .matchFile(thisName, files))
        system.file("Examples", found, package = package)
    }
}

## Utility functions used by the previous

.moreSource <- function(demo) {
            (demo@pos < length(demo@lines) || (length(demo@partial) > 0))
}

.sourceAsDemo <- function(source) {
    demo <- new("demoSource")
    if(is.character(source))
      source = file(source)
    else if(!inherits(source, "connection"))
      stop("source must be  a connection or the name of a file (got class \"",class(source),
           "\"")
    if(!isOpen(source)) {
      open(source, "r")
      on.exit(close(source), add = TRUE)
    }
    demo@lines <- readLines(source)
    demo@pos <- 0
    demo
}



.evalWithVisible <- function(demo) {
    exprs <- demo@expr
    envir <- demo@envir
    value <- NULL
    for(expr in exprs) {
        exp2 <- substitute(withVisible(expr))
        val <- try(eval(exp2, envir = envir, enclos = parent.env(envir)))
        if(inherits(val, "try-error")) {
            demo@value <- val
            demo@state <- "error"
            return(demo)
        }
        value <- val$value
        if(val$visible)
          print(value)
    }
    demo@value <- value
    demo@state <- "evaluated"
    demo
}

.tryParse <- function(text) {
    tt = trySilent(parse(text = text))
    if(inherits(tt, "try-error")) {
        if(grep(fixed = TRUE, "unexpected end of input", tt) > 0)
          tt
        else {
            message(tt)
            expression() # do nothing
        }
    }
    else
      tt
}

.nextSourceLine <- function(demo) {
    pos <- demo@pos
    prompt <- getOption("continue")
    while(pos < length(demo@lines)) {
      pos <- pos+1
      line <- demo@lines[[pos]]
      if(regexpr("#SILENT$", line)>0) {
          if(.nonCommentLine(line)) {
              demo@pos <- pos
              prevState <- demo@state
              save <- demo@partial
              demo@partial <- line
              demo <- .parseDemo(demo)
              demo <- .evalWithVisible(demo)
              demo@partial <- save
              demo@state <- prevState
          }
          next
      }
      cat(line, "\n", sep="", file = stderr())
      demo@partial <- c(demo@partial, line)
      demo@pos <- pos
      if(.nonCommentLine(line)) {
          demo@state <- "partial" # ?? probably will be set by caller
          break
      }
      .showPrompt("partial")
    }
    demo
}

.keyStrings <- list(
                    complete = ".",
                    continue = ",",
                    quit = "q"
                    )

.doParseEval <- function(demo,  eval = NA, read = TRUE) {
    if(is.na(eval)) {
        ## alternately parse and evaluate
      if(identical(demo@state, "parsed"))
          return(.evalWithVisible(demo))
      else
          eval <- FALSE # do only the parse step this time
    }
    while(.moreSource(demo)) {
        if(read)
          demo <- .nextSourceLine(demo)
        demo <- .parseDemo(demo)
        if(identical(demo@state,"parsed")) {
            if(eval)
                 demo<- .evalWithVisible(demo)
            break ## finished either a parse or a parse-eval step
        }
    }
    demo
}

.parseDemo <- function(demo) {
    expr <- .tryParse(demo@partial)
    if(inherits(expr, "try-error")) {
            demo@expr <- expression()
            demo@state <- "partial"
            .showPrompt("partial")
        }
    else {
        demo@state <- "parsed"
        demo@partial <- character()
        demo@expr <- expr
    }
    demo
}

.showPrompt <- function(state) {
    switch(state,
           partial =  cat(getOption("continue"), file = stderr()),
           parsed = return() ,# do nothing
          # else, first time or evaluated or something else, like an error
           cat(getOption("prompt"), file = stderr())
           )
}

.getNextLine <- function(inputCon, pause = 1) {
    repeat {
        txt <- readLines(inputCon, 1)
        if(length(txt) > 0)
          return(txt)
        ## FIXME:  would be better to use isIncomplete() here
    }
}

.nonCommentLine <- function(line)
 (regexpr("^[[:blank:]]*($|#)", line) < 0)

demoInput <- function(path = "./DemoSourceFifo") {
    con  <- .demoFifo(path, "w")
    repeat{
        control <- readLines(n=1)
        cat(control, "\n", sep="", file = con)
        if(identical(control, "q" ))
          return(invisible())
    }
}

.demoFifo <- function(path = "./DemoSourceFifo", open = "r") {
    if(identical(open, "r"))
      fifo(path, open)
    else
      file(path, open) # usually "w" to truncate
}

.previewLine <- function(demo, file) {
    line <- demo@lines[demo@pos+1]
    if(!is.na(line))
      cat("## ", line, "\n", file = file)
}
