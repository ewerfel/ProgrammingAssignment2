## These functions will cache the inversion of a matrix so that if 
## invoked more than once, the cached matrix will be returned.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  my.inverse <- NULL
  set <- function(y) {
    x <<- y
    my.inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(matrix_inverse) my.inverse <<- matrix_inverse
  getInverse <- function() my.inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.  
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  my.inverse <- x$getInverse()
  if(!is.null(my.inverse)) {
    message("getting cached data")
    return(my.inverse)
  }
  data <- x$get()
  my.inverse <- solve(data, ...)
  x$setInverse(my.inverse)
  my.inverse
}
