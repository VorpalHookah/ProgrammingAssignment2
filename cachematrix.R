## The two functions saves time-comsuming computation: Matrix Inverse,
## by caching the inverse matrices that are already computed.
## They first check if the input matrix and its inverse are already cached.
## If so, they return the cached inverse matrix directly.
## If it is not cached, the functions will calculate the inverse matrix
## and save the matrix into cache and finally returns the result.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## Doesn't Calculate but saves matrix to variable x and inverse to variable k.

makeCacheMatrix <- function(x = matrix()) {
        k <- NULL
        set <- function(y) {
                x <<- y
                k <<- NULL
        }
        get <- function() {
                x
        }
        setmatrix <- function(solve) {
                k <<- solve
        }
        getmatrix <- function() {
                k
        }
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## cacheSolve takes a caching matrix created with makeCacheMatrix,
## then either return the cached inverse of it, if inverse was already cached;
## or calculates the inverse for the mastrix saved in 'x', and returns result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        k <- x$getmatrix()
        if(!is.null(k)) {
                message("getting cached data")
                return(k)
        }
        data <- x$get()
        k <- solve(data, ...)
        x$setmatrix(k)
        k
}
