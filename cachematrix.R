## Computing the inverse of a square matrix without recalculation if the
## matrix has not been changed.

## Creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverseOfx <- NULL
        set <- function(y) {
                x <<- y
                inverseOfx <<- NULL
        }
        get <- function() x
        setInverse <- function(i) inverseOfx <<- i
        getInverse <- function() inverseOfx
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Computes the inverse of the "matrix" returned by makeCacheMatrix.
## The result will be cached if the underlying matrix has not been reset.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseOfx <- x$getInverse()
        if (!is.null(inverseOfx)) {
                return(inverseOfx)
        }
        data <- x$get()
        inverseOfx <- solve(data, ...)
        x$setInverse(inverseOfx)
        inverseOfx
}
