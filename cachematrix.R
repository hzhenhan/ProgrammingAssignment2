## Put comments here that give an overall description of what your
## functions do
## makeVector stores a matrix (x) in the memory.
## cachemean gives inverse of the matrix if it is in the memory or computes the inverse and then shows the inverse.

## Write a short comment describing this function
## this function(makeVector) uses scoping rules and stores matrices in memory.
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

## Write a short comment describing this function
## the input x must be a makeVector and this function(cachemean) returns the inverse of the matrix.
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
