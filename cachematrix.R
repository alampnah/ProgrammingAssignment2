## 
## functions do

## makeCacheMatrix 

makeCacheMatrix <- function(x = matrix()) {
    inverseOfX <- NULL
    set <- function(y) {
        x <<- y
        inverseOfX <<- NULL
    }
    get <- function() x
    setCacheSolve <- function(invofx) inverseOfX <<- invofx
    getCacheSolve <- function() inverseOfX
    list(set = set, get = get,
         setCacheSolve = setCacheSolve,
         getCacheSolve = getCacheSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseOfX <- x$getCacheSolve()
    if(!is.null(inverseOfX)) {
        message("getting cached data")
        return(inverseOfX)
    }
    orgMatrix <- x$get()
    inverseOfX <- solve(orgMatrix, ...)
    x$setCacheSolve(inverseOfX)
    inverseOfX
}
