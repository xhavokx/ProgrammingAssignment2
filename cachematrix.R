## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
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


## Write a short comment describing this function

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


makeVector <- function(x = numeric()) {
    # m is the place where we'll store the mean of the vector after it has been calculated
    # m is initially NULL because we have not calculated the mean - NULL indicates an empty value
    m <- NULL
    
    # set is a function that we'll call to "set" the value of the vector
    # when called this uses the double headed arrow to assign to a variable which was declared "outside" the scope of this function
    set <- function(y) {
        # Set the new value for the vector
        x <<- y
        # set m to NULL, to "reset" the mean (since the vector has itself changed, the old mean is no longer valid)
        m <<- NULL
    }
    # Get the current value of the vector
    get <- function() x
    # set the value of m, the mean of the vector "x". Using the double headed arrow, as m is declared outside of this function
    setmean <- function(mean) m <<- mean
    # get the current mean, which may return NULL if the mean has not been calculated and set
    getmean <- function() m
    
    # package up the 4 functions into a list to return to the caller
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

# x is the result of calling makeVector - a list of 4 functions
cachemean <- function(x, ...) {
    # get the current value of the mean
    m <- x$getmean()
    
    # if m is not null, we can return the value, as we've previously calculated it
    if(!is.null(m)) {
        print("getting cached data")
        return(m)
    }
    
    # else get the vector
    data <- x$get()
    # calculate the mean
    m <- mean(data, ...)
    # set the mean in the "makeVector" list of functions
    x$setmean(m)
    
    # and return the mean we just calculated
    m
}
