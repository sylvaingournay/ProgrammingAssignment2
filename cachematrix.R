## Module to allow matrix inversion caching.

## Creates a structure containing a matrix and its inverse (if computed),
## and methods to set/get the matrix and its inverse

makeCacheMatrix <- function(mx = matrix()) {
    s <- NULL
    set <- function(my) {
        mx <<- my
        solve <<- NULL
    }
    get <- function() mx
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s

    list(set = set, get = get,
         setSolve = setSolve, getSolve = getSolve)
}


## Retrieve the inverse of the matrix defined in mx if it has already been
## computed or compute it

cacheSolve <- function(mx, ...) {
    if (!is.null(mx$getInvert())) {
        return(mx$getInvert())
    }
    s <- solve(mx$get())
    mx$setInvert(s)
    s
}
