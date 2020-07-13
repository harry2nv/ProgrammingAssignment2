## Put comments here that give an overall description of what your
## functions do

##Caching the Inverse of a Matrix:
  
##Matrix inversion is usually a costly computation and there may be some
##benefits to cache the inverse of a matrix rather than to compute it repeatedly.
##Below is a pair of functions which is used to create a special object that 
##stores a matrix and caches its inverse.


## Write a short comment describing this function

#This function creates a special "matrix" object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

##This function computes the inverse of the special "matrix" created by
##makeCacheMatrix. If the inverse has already been calculated (and the
##matrix has not changed), then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    inv <- x$getInverse()
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
  
}



