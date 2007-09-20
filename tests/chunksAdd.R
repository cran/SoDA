if(tryCatch(require(RSPerl, quietly = TRUE), error = function(cond)FALSE)) {
set.seed(314)
someLetters <- sample(letters, 100, TRUE)
tbl <- chunksAdd(data = someLetters[1:50])
tbl <- chunksAdd(tbl, someLetters[51:100])
tbl <- chunksDrop(tbl, someLetters[1:10])
chunksAdd(tbl) # to convert the table
}
