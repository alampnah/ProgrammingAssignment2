## The following pair of functions help optimize the 'solve' function calls on a matrix. If the matrix data
## is not modified then multiple calls for computing it's inverse are optimized by caching the result from
## the first call. In a nutshell the functions below emulate a singelton object for a matrix. 

## A better implementation would have been a single function that returns a list of get,set and solve 
## functions. This suggestion is beyond the scope of this project

## makeVector: This function creates a special "matrix", which is really a list containing functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverseOfX <- NULL
    set <- function(y) {
        x <<- y
        ## If the matrix changes the inverse has to be recomputed so the cache is purged
        ## The inverse is set to NULL and only computed when cacheSolve is called (lazy computation)
        inverseOfX <<- NULL
    }
    get <- function() x
    setCacheSolve <- function(invofx) inverseOfX <<- invofx
    getCacheSolve <- function() inverseOfX
    list(set = set, get = get, setCacheSolve = setCacheSolve, getCacheSolve = getCacheSolve)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then cachesolve
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inverseOfX <- x$getCacheSolve()
    if(!is.null(inverseOfX)) {
        ## getting cached data
        return(inverseOfX)
    }
    orgMatrix <- x$get()
    if (nrow(orgMatrix)!=ncol(orgMatrix))
    {
        stop("need a square matrix")
    }
    inverseOfX <- solve(orgMatrix, ...)
    x$setCacheSolve(inverseOfX)
    inverseOfX
}
