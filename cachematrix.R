## These functions set a matrix and then allow for them to cache the different versions of them.

## This function allows to set up a matrix cacheing the different versions and functions along the way.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  #We start by setting that the value of the matrix as a NULL value
  #We will then set the value of the matrix
  setmtrx <- function(y){
    x <<- y
    inverse <- NULL
  }
  getmtrx <- function() {x}
  setInversemtrx <- function(invert) {inverse <<- invert}
  getInversemtrx <- function() {inverse}
  #Here we set up the different cache functions that we will then recall
  list(setmtrx = setmtrx, getmtrx=getmtrx, setInversemtrx=setInversemtrx, getInversemtrx=getInversemtrx)
}


## This function obtains the inverse of the matrix that was obtained with the previous function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInversemtrx()
  if(!is.null(inverse)){
    message("Copying data from cache")
    return(inverse)
  }
  mtrx <- x$getmtrx()
  inverse <- solve(mtrx, ...)
  x$setInversemtrx(inverse)
  inverse
}
