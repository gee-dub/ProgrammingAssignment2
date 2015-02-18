## These functions are used to optimize repeated calls for the inverses of matrices by
## caching the inverse.

## Usage:
## im <- matrix(data = c(4, 3, 3, 2), nrow = 2, ncol = 2)
## m <- makeCacheMatrix(im)
## cacheSolve(m)
## cacheSolve(m)
## cacheSolve(m)

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above, leveraging the cache where possible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}

