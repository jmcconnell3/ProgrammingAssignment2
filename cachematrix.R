## Submission for Coursera R Programming course, Programming Assignment 2
## Assignment on Lexical Scoping - Caching the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse.
## Pass in a matrix, and the return is an object containing the matrix data and
## a cache of its inverse(if already computed).  Access the data via the 
## supplied get/set methods for the data and the inverse.

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        ## set x to the new matrix, NULL the cache as it is no longer valid
        x <<- y
        m <<- NULL 
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinv() ## see if there is  a value in the cache for the inverse
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## if not, get the matrix, call "solve" on it, then cache the result
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
