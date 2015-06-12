## cachematrix.R includes the functions makeCacheMatrix() and cacheSolve() 
## which enable more rapid solution of the inverses of matrices through caching
## the caching is supported through the lexical scoping properties of R

##Execution instructions:
##source() this file
##call makeCacheMatrix() passing a defined square matrix (invertible) as argument
##and assigning output of makeCacheMatrix() to a new object
##call cacheSolve() passing object containing output of makeCacheMatrix() (which is a list) as argument
##inverse of original matrix should be returned, cached or not, unless it was not invertible

## makeCacheMatrix creates a matrix object that can cache its inverse

##An additional test line for commit to GitHub on new machine 12/06/15
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
      x
    }
    setinv <- function() {
      inv <<- solve(x)
    }
    getinv <- function() {
      inv
    }
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}

## cacheSolve takes a matrix and returns its inverse
## if the matrix has been cached using makeCacheMatrix
## the matrix will be withdrawn from the cache and its inverse returned
## if not, the matrix will be part of the input
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    else {
        data <- x$get()
        inv <- x$setinv()
        return(inv)
    }  
}
