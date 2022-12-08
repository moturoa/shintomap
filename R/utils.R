

random_id <- function(n=8){
  paste(sample(letters,n),collapse="")
}

dropNulls <- function (x){
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}
