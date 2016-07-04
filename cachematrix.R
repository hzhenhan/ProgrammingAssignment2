## Put comments here that give an overall description of what your
## functions do
## makeVector stores a matrix (x) in the memory.
## cachemean gives inverse of the matrix if it is in the memory or computes the inverse and then shows the inverse.

## Write a short comment describing this function
## this function(makeVector) uses scoping rules and stores matrices in memory.
makeVector <- function(x = numeric()) {
  v <- NULL
  set <- function(y) {
    x <<- y
    v <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) v <<- inverse
  getinverse <- function() v
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
## the input x must be a makeVector and this function(cachemean) returns the inverse of the matrix.
cachemean <- function(x, ...) {
  v <- x$getinverse()
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  data <- x$get()
  v <- solve(data, ...)
  x$setinverse(v)
  v
}
