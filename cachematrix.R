## This script contains two functions allowing us to cache the inverse of 
## a matrix rather than to compute it repeatedly.

## Creates a matrix object that can cache its inverse.
## The function returns a list of functions that allow us to set / get values of
## the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
   cachedInverse <- NULL
   
   set <- function(y) {
     x <<- y
     cachedInverse <<- NULL
   }
   get <- function() x
   setInverse <- function(invertedMatrix) cachedInverse <<- invertedMatrix
   getInverse <- function() cachedInverse
   
   list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of a matrix. If the inverse has already been 
## calculated then the function returns the value from the cache. 
## Otherwise the inverse is computed using the solve function.
## The parameter x has to be a matrix returned by the makeCacheMatrix function.
## Parameters ... are passed to the solve function
cacheSolve <- function(x, ...) {
  cachedInverse <- x$getInverse()
  if(!is.null(cachedInverse) ) {
    print("ASD")
    message("getting cached data")
    cachedInverse
  }
  else {
    invertedMatrix <- solve(x$get(), ...)
    x$setInverse(invertedMatrix)
    invertedMatrix
  }
}
