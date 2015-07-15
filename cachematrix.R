## This file provides a set of functions for caching the inverse of a matrix, for performance reasons.

## Creates an object that represents a cached matrix. It exposes the following functions:
## - set: set the value of the matrix
## - get: get the value of the matrix
## - setinverse: set the inverse of the matrix
## - getinverse: get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of a matrix, preferabily through its cached value.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse  
}
