## Put comments here that give an overall description of what your
## functions do

## A factory function which yields an object encapsulating a square invertible matrix x,
## with the following methods:
##
##   get(), retrieves the current matrix; set( m) where m is assumed to be a square matrix;
##   getinv(), retrieves the inversion of x, or NULL if it has not been set.
##   setinv( inv) which assigns inv to the value returned by getinv(), but only if getinv() is NULL
##   set( new_x), sets the square invertible matrix to be new_x, unless x is already assigned, 
##   in which case nothing is done. 
##
## If provided, x is assumed to be a square invertible matrix and can't be changed in the returned object. 
## Otherwise the default is a 1 by 1 empty matrix which can be overwritten by invoking set().
## Note that the matrix inversion computation is not itself provided, only its storage. Therefore, 
## one should check for getinv() yielding NULL before using its value, and/or assign the inverse by calling
## x$setinv.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # the inverse of x

    set <- function( y) {
            x <<- y
            i <<- NULL
        }

    get <- function() x

    setinv <- function( inv) i <<- inv 
    getinv <- function() i
    
    list( set = set, 
        get = get, 
        setinv = setinv, 
        getinv = getinv)
}


## Yields the inverse of the x$get(), computing it if necessary. In both cases, the inverse will 
## be returned by future invocations of  x$getinv().

cacheSolve <- function(x, ...) {

        ## Return current inverse if there is one
        cached <-  x$getinv()
        if( !is.null( cached))
            return( cached) 

        ## Cache the result and yield
        x$setinv( solve(x$get(), ...))
        x$getinv()
}
        
