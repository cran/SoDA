doQuantile <- function(oldQ, newdata, probs) {
    if(!exists("hiddenData"))
      hiddenData <<- numeric()
    hiddenData <<- c(hiddenData, newdata)
    quantile(hiddenData, probs)
}
