## The following functions can be used to optimize
## the repeated computation of the inverse of a matrix
## The caching mechanism avoids the inverse matrix to be 
## computed more than once

## Creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of a matrix returned by the makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed), 
## cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
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