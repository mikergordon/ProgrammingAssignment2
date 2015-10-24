## Overall description of functions
## Pair of functions that cache the inverse of a matrix.
## makeCacheMatrix: Creates a special "matrix" object
## that can cache its inverse.
## cacheSolve: Computes the inverse of the special
## "matrix" returned by makeCacheMatrix.

## makeCacheMatrix description
## Creates a special matrix that can cache its inverse, which is
## a list containing:
## 1. set value of matrix
## 2. get value of matrix
## 3. set value of inverse of matrix (solve)
## 4. get value of inverse of matrix (solve)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve description
## Calculate inverse of matrix created in makeCacheMatrix function
## First check if the inverse of matrix is cached.
## If cached, skip calculation and return the cached value.
## If not cached, calculate the inverse (solve) of the data
## and cache for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
