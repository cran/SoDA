strictOp <- function(expr, warnOnly = FALSE, errorCall = substitute(expr)) {
    condFun <- if(warnOnly) warning else stop
    fCall <- substitute(expr)
    if(!is.language(fCall)) {
        warning("expression supplied to strictOp() was not a function call")
        return(expr)
    }
    fname <- fCall[[1]]
    if(length(fCall) != 3) {
       warning("expression supplied to strictOp() was not a simple binary operator call: ",
              deparse(fCall) )
        return(expr)
    }
    if(is.name(fname)) {
        f <- as.character(fname)
        gen = getGeneric(f)
    }
    else {
        gen <- getGeneric(eval.parent(fname)) # e.g., works for base::`+`
        if(is.null(gen) ) f <- deparse(fname)
        else f <- gen@generic
        fCall[[1]] <- as.name(f) # for strictOp error messages use the actual function
    }
    e1 <- eval.parent(fCall[[2]])
    e2 <- eval.parent(fCall[[3]])
    ## From now on, must evaluate the expression locally to avoid re-evaluating
    ## one of the operands.
    localCall <- fCall
    localCall[[2]] <- quote(e1); localCall[[3]] <- quote(e2)
    if(is.null(gen)) {
        warning("Function \"", f, "\" is not one of the operators that can be checked")
        return(eval(localCall))
    }
    ## Analyse the operators according to their group
   group = gen@group
    if(length(group) == 0) {
        ## only && and || have no group
        if(identical(f, "&&") || identical(f, "||"))
          .strictCondOp(f, e1, e2, eval(localCall), errorCall, condFun)
        else {
            warning("Function \"", f, "\" is not one of the operators that can be checked")
            return(eval(localCall))
        }
    }
    g1 = group[[1]]
    if(!(is.character(g1) && length(g1) == 1)) {
        warning("Function \"", f, "\" has a non-standard generic group")
        return(eval(localCall))
    }
    switch(g1,
           Arith = .strictArithOp(f, e1, e2, eval(localCall), errorCall, condFun),
           Logic = .strictLogicOp(f, e1, e2, eval(localCall), errorCall, condFun),
           Compare = .strictCompareOp(f, e1, e2, localCall, errorCall, condFun),
           {
               warning("Function \"", f, "\" is not one of the operators that can be checked")
               return(eval(localCall))
           })
}

`.tail<-` <- function(x, value) {
    c(x, paste(value, collapse = ""))
}

## support functions for the various groups

## Condition operators:  both operands must be single logical values
.strictCondOp <- function(f, e1, e2, expr, errorCall, condFun) {
    msg <- character()
    if(!is.logical(e1))
      .tail(msg) <- c("First argument is not logical: class \"", class(e1), "\"")
    else if(length(e1) != 1)
      .tail(msg) <- c("Length of first argument shojuld be 1; got ", length(e1))
    else if(is.na(e1))
      .tail(msg) <- c("First argument is NA")
    if(!is.logical(e2))
      .tail(msg) <- c("Second argument is not logical: class \"", class(e2), "\"")
    else if(length(e2) != 1)
      .tail(msg) <- c("Length of second argument shojuld be 1; got ", length(e2))
    else if(is.na(e2))
      .tail(msg) <- c("Second argument is NA")
    if(length(msg) > 0)
       condFun("<strictOp>: ", deparse(errorCall)[[1]], ": ", msg, call. = FALSE)
    return(expr)
}

.strictLogicOp <- function(f, e1, e2, expr, errorCall, condFun) {
    is.raw <- function(x)identical(typeof(x), "raw")
    msg <- character()
    if(!is.logical(e1) && !is.raw(e1))
      .tail(msg) <- c("First argument is not logical or raw: class \"", class(e1), "\"")
    if(!is.logical(e2) && !is.raw(e2))
      .tail(msg) <- c("Second argument is not logical or raw: class \"", class(e2), "\"")
    if(length(msg) > 0)
       condFun("<strictOp>: ", deparse(errorCall)[[1]], ": ", msg, call. = FALSE)
    return(expr)
}

.checkDataType <- function(x)
    switch(typeof(x),
           double = , integer = "numeric",
           character = "character",
           logical ="logical",
           complex = "complex",
           raw = "raw",
           "other")
        
.strictCompareOp <- function(f, e1, e2, expr, errorCall, condFun) {
    msg <- character()
    typeCheck = paste(.checkDataType(e1), .checkDataType(e2), sep=".")
    switch(typeCheck,
           numeric.numeric = , character.character = ,
           logical.logical = , complex.complex = ,
           raw.raw = {},
           .tail(msg) <- c("Undefined combination of types for comparison: ", typeof(e1),
                           ", ", typeof(e2))
           )
    l1 = length(e1); l2 = length(e2)
    if(l1 != l2 && l1 != 1 && l2 != 1)
      .tail(msg) <- c("Ambiguous unequal lengths: ", l1, ", ", l2)
     if(length(msg) > 0)
       condFun("<strictOp>: ", deparse(errorCall)[[1]], ": ", msg, call. = FALSE)
    return(eval.parent(expr))
}

.strictArithOp  <- function(f, e1, e2, expr, errorCall, condFun) {
    msg <- character()
    typeCheck = paste(.checkDataType(e1), .checkDataType(e2), sep=".")
    switch(typeCheck,
           numeric.numeric = , numeric.complex = ,
           complex.numeric = {},
           .tail(msg) <- c("Undefined combination of types for arithmetic: ", typeof(e1),
                           ", ", typeof(e2))
           )
    l1 = length(e1); l2 = length(e2)
    if(l1 != l2 && l1 != 1 && l2 != 1)
      .tail(msg) <- c("Ambiguous unequal lengths: ", l1, ", ", l2)
     if(length(msg) > 0)
       condFun("<strictOp>: ", deparse(errorCall)[[1]], ": ", msg, call. = FALSE)
    return(eval.parent(expr))
}


.makeStrictEnv <- function() {
    myEnv <- environment(sys.function())
    doOp <- function(f) {
            op <- args(f)
            if(!is.null(op)) {
              body(op) <- substitute(strictOp(base::WHAT(e1, e2), errorCall = sys.call()),
                                       list(WHAT = as.name(f)))
              assign(f, op, envir = .strictEnv)
          }
        }
    for(group in c("Arith", "Logic", "Compare")) {
        groupGen <- getGeneric(group)
        for(f in getGroupMembers(groupGen))
            doOp(f)
    }
    for(f in c("&&", "||"))
        doOp(f)
}

.strictEnv <- new.env(TRUE)

.makeStrictEnv()

withStrictOps <- function(expr, attach = TRUE) {
    if(missing(expr)) {
        if(identical(attach, TRUE))
          attach(.strictEnv, name = "srictOps", warn.conflicts = FALSE)
        else if(identical(attach, FALSE)) {
          pos <- match("strictOps", search())
          if(is.na(pos))
            message("strictOps not attached, no action taken")
          else {
              detach(pos)
              message("strictOps environment detached")
          }
      }
    }
    else
        eval(substitute(expr), .strictEnv, enclos = parent.frame())
}
