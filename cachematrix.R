## The following functions create a R object being able to store a matrix
## and to cache its inverse.

## The function 'makeCacheMatrix' defines four functions to manage the call and
## storage of a matrix object and its inverse and returns their list

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inversematrix) inverse <<- inversematrix
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The cacheSolve function calculates the inverse matrix of the object created 
## through 'makeCacheMatrix' by means of the solve() function.
## Before the computation, however, it checks whether the inverse has already
## been calculated and if this happens, it retrieves the inverse matrix from 
## the cache and returns it without making any calculation.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    tobeinverted <- x$get()
    inverse <- solve(tobeinverted, ...)
    x$setinverse(inverse)
    inverse
}
