## These functions demonstrate how to use caching within an object environment
## to store the results of a costly operation so that it only has to be
## performed a single time (assuming the input remains the same).

## makeCacheMatrix() returns an environment which contains a matrix (the input),
## and 4 helper functions to modify or read the value or inverse of that matrix.
## The actual output of the function is a list, which contains these helper
## functions, and also has the values of the matrix and its inverse cached.
## The helper functions are called as regular list items, e.g. a$get()

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL # Instantiate inverse variable as NULL to show it's not set.
        set <- function(y) { # set function assigns new matrix to x
                x <<- y
                i <<- NULL # Reset inverse when new x value set
        }
        get <- function() x # get returns matrix x
        setInverse <- function(inverse) i <<- inverse # set the inverse matrix
        getInverse <- function() i # return the inverse matrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse) # the output of the function, a list of functions.
}


## Return a matrix that is the inverse of input x, which is a CacheMatrix as
## defined by the above function. If the inverse of x has already been calculated
## and set (cached) in the object, it is simply retrieved and returned.
## If the inverse has not yet been calculated, it is calculated, then cached.

cacheSolve <- function(x, ...) {
        i <- x$getInverse() # get inverse of the input object
        if(!is.null(i)) { # if the inverse is already set...
                message("getting cached data") # retrieve the value
                return(i) # return the value
        }
        data <- x$get() # if inverse is not yet set, retrieve matrix to be inverted.
        i <- solve(data, ...) # calculate inverse and set variable i to the result.
        x$setInverse(i) # cache result so future calculations can be avoided.
        i # return inverse
}
