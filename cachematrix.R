## This file provides functions for creating a manipulating an
## an object that wraps a matrix allowing the matrix inverse to 
## be cached.
##
## To use the functions you first create a matrix and 
## wrap it using 'makeCacheMatrix'
#
##  m <- matrix(c(1,0,0,0,1,0,0,0,1), nrow = 3, ncol = 3)
##  cm <- makeCacheMatrix(m)
##
## To calculate and cached the inverse use cacheSolve
##
## mi <- cacheSolve(cm)
##
## The value of the wrapped matrix can be retrieved as follows:
##
## matrixValue = cm$get()
##
## and the value of the inverse:
##
## matrixInverse = cm$getinverse()
##
## Note that the getinverse could return null if cacheSolve has not
## been called.
##
## The value of the wrapped matrix can be changed using
##
## cm$set(anotherMatrix)
##
## The setinverse call should only be called by cacheSolve.
##

#' Creates an "object" wrapping a matrix allowing
#' the inverse of the matrix to be cached
#'
#' @param x the matrix to be wrapped
#' @return a list of functions for manipulating the matrix
#' @seealso cacheSolve
makeCacheMatrix <- function(x = matrix()) {
    # The cached inverse of the matrix x
    cachedInverse <- NULL
    
    # Set the matrix taking care to invalidate the cached value
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    
    # Retrieve the value of the matrix x
    get <- function() x
    
    # Update the cached inverse value
    # See cacheSolve
    setinverse <- function(inverse) cachedInverse <<- inverse
    
    # Retrieve the cachedInverse
    getinverse <- function() {
        cachedInverse
    }
    
    # Return a list of the functions to manipulate this object
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}



#' Retrieves the inverse of a matrix using the cached 
#' value or by calculating it, if necessary.
#' 
#' It if is necessary to calculate the inverse the result
#' is cached for future calls.
#' 
#' @param x and wrapped matrix obtained from a call to makeCacheMatrix
#' @return the inverse of the matrix wrapped by x
#' @seealso makeCacheMatrix
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("returning cached inverse")
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix)
    x$setinverse(inverse)
    inverse
}
