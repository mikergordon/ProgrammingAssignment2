## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix description
## Creates a special matrix taht can cache its inverse, which is a list containing:
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
## Calculates inverse of matrix created in makeCacheMatrix function
## First it checks if the inverse of matrix is cached.
## If it is cached, it skips calculation and returns the cached value.
## If it is not cached, it calculates the inverse (solve) of the data and
## caches it for the future.

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
