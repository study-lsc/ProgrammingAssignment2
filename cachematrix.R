## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function returns a set of functions (accessed as members of a list)
## These functions allow you to set a matrix, get the matrix, set the inverse of the matrix, and get
## the stored inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    set <- function(y) {
        x <<- y
        inverted <<- NULL
    }
    get <- function() x
    setinverse <- function(computed_inverse=matrix()) inverted <<- computed_inverse
    getinverse <- function() inverted
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## This function takes as input an object created by makeCacheMatrix
## It will determine if the matrix stored by makeCacheMatrix alreay has a inverse matrix computed.
## If no inverse matrix is already computed, it will compute the inverse and store it using the setinverse
## function of makeCacheMatrix.  If the inverse already exists, it will use the cached inverse matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverted <- x$getinverse()
    if(!is.null(inverted)) {
        message("getting cached data")
        return(inverted)
    }
    data <- x$get()
    inverted <- solve(data, ...)
    x$setinverse(inverted)
    inverted
}
