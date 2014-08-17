## This file contains a functions for creating and calculating the 
## inverse of special "matrix" objects which are capable of caching 
## their inverses.  The inverse of a matrix can be quite expensive to 
## calculate for large matrixes and caching the results can speed things
## up if the inverse of the same matrix is required multiple times.

## makeCacheMatrix creates a special matrix which is capable of caching its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {
        x  
    } 
    set_inverse <- function(inv) {
        inverse <<- inv
    }
    get_inverse <- function() {
        inverse
    }
    
    list(set = set,
         get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$get_inverse()
    if (is.null(inv)) {
        data <- x$get()
        inv <- solve(data)
        x$set_inverse(inv)
    }
    inv
}

