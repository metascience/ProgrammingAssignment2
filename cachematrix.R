## cachematrix.R
##
## This file provides functions for caching the inverse of a matrix
## alongside the matrix itself. the benefit of doing this is that the
## inverse can be accessed whenever needed, but the expensive inversion
## operation is only performed once.
##
## The makeCacheMatrix function is used for creating a representation
## of the matrix, and the cacheSolve function is used for retrieving the
## solved inverse. For example, you can use them as follows:
##
##    mymat <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
##    inv <- cacheSolve(mymat)  # solves and returns the inverse
##    inv <- cacheSolve(mymat)  # 2nd call quickly returns the cached inverse
##
## Created for R Programming, Assignment 2.

## Creates a special "matrix" which is really a list of functions
## allowing the storage of a matrix and its cached inverse.
makeCacheMatrix <- function(x = matrix()) {
  # The inv variable will hold the matrix inverse once computed.
  # Initialize it to NULL so that we can check if it has been cached.
  inv <- NULL
  
  # Create functions for setting and retrieving the matrix and its cache.
  set <- function(y) {
    # The matrix (x) and its inverse (inv) will be stored
    # in the environment of the function returned by
    # makeCacheMatrix, using the <<- operator to skip one level.
    x <<- y
    
    # Clear the inverse (important if we have used set to replace an
    # old matrix with a new one, in which case the old cached inverse
    # would be invalid for the new matrix)
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  # Put all of these functions into a list and return it, so that
  # the user can call them later
  list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Returns the inverse of the "matrix" created with the above function.
## If the inverse has not been calculated yet, it will be solved and
## the result will also be cached. If it has already been solved before,
## the cached result will be returned. Arguments include the matrix
## from the above function as well as any optional arguments to the
## solve() function for calculating the inverse.
cacheSolve <- function(x, ...) {
  # Get the cached inverse from the matrix "object"
  inv <- x$getinverse()
  
  # If the inverse has not been computed yet, the cache value will be NULL.
  if(!is.null(inv)) {
    # The inverse was in the cache, so we can just return it and be done.
    message("getting cached data")
    return(inv)
  }
  
  # If we get this far, the inverse is not in the cache and needs to be
  # computed.
  mat <- x$get()
  inv <- solve(mat, ...)
  
  # Put the inverse the cache so we can get it later
  x$setinverse(inv)
  
  # Finally, return the inverse.
  inv
}
