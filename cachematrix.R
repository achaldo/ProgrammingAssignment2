## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list of four functions
## these functions either get or set the value of the
## matrix or inverse. These functions are then used
## by the function below.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve takes as an argument, a list created by
## the function makeCacheMatrix. It then uses the elements
## (which are functions) of the list to either calculate
## the inverse or if it's already been calculated it
## return the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}