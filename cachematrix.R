## This pair of functions creates a special object that stores
## a square invertible matrix and caches its inverse. This way
## if the same matrix is passed to the function, it avoids
## having to run the computation again and can retrieve the
## result from cache.

## The makeCacheMatrix function creates a special matrix
## object that stores the matrix as well as its cached
## inverse.
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

## cacheSolve calculates the inverse of the matrix if it has
## not been calculated before, or returns the result from
## cache if it has been previously calculated.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setsolve(m)
    m
}
