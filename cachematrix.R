## These functions are very similar to the makeVector and cachemean functions in the question.
## In "makeCacheMatrix" I have simply changed the labelling exchanging m for inv and mean for "invmatrix"
## In "cacheSolve" I have done the same, and also switched the "mean" function for the "solve" funtion.

## This function stores the matrix object as well as a cached inverse and provides the subsidiary funtions to define and retreive both of them.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinvmatrix <- function(invmatrix) inv <<- invmatrix
    getinvmatrix <- function() inv
    list(set = set, get = get, setinvmatrix = setinvmatrix, getinvmatrix = getinvmatrix)

}


## This function checks if there is a cached inverse matrix present. If there is it returns it. Otherwise it computes it and prints it out.

cacheSolve <- function(x, ...) {
    inv <- x$getinvmatrix()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get ()
    inv <- solve(data, ...)
    x$setinvmatrix(inv)
    inv
}
