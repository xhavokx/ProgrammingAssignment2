## This R script contains 2 functions which work together to provide the ability to cache the result of calling the solve function on a matrix


## This function returns a list of 4 closures which share the environment of the parent function call, specifically to access a matrix and a cache of it's inverse
## This shared environment allows the caching of the results of calling the solve
## Parameters:
##  x: input matrix to be cached
## Returns a list of 4 closures which manipulate the environment created by the invocation of this function

makeCacheMatrix <- function(x = matrix()) {
    c <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(mean) m <<- mean
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This functions takes a list of 4 closures, as returned by the makeCacheMatrix function,
## and using those calculates the inverse using the solve function. The function initially
## checks to see if the inverse has previously been calculated, and if so will use that
## cached value, otherwise it will calculate the inverse, cache it using the provided
## closures, and return the inverse matrix

cacheSolve <- function(x, ...) {
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