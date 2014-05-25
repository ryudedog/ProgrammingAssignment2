## Put comments here that give an overall description of what your
## functions do
##
## Matrix inversion is usually a costly computation and their may be
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly.  The enclosed functions provide caching of the inverse
## of a matrix.  The functions assume that the supplied matrix is
## always invertable.
##

## Write a short comment describing this function
##
## makeCacheMatrix: This function creates a special "matrix" object that
## can cache its inverse.
##
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  setinverse <- function(sVal) i <<- sVal
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##
## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

