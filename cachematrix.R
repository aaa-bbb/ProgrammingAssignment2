## Coursera
## R Programming 
## Programming assignment 2
## Purpose: write an R function able to cache potentially time-consuming computations (inversing a matrix)


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}   


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
    ## If the inverse has already been calculated then return the value
    inv <- x$get_inverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## if the inverse hasn't yet been calculated then calculate it and cache the value
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inverse(inv)
    inv
}
