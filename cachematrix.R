## A pair of functions which implement the encapsulation and caching of a matrix
## and its inverse's computation.   
## 
## makeCacheMatrix is a factory function yielding an object providing access methods to 
## retrieve and assign the matrix and its inverse. 
## 
## cacheSolve takes an object obtained via makeCacheMatrix and returns the inverse if already computed;
## otherwise the inverse is computed and cached by invoking setinv() on its argument prior to being 
## returned.
##
## It is important to note that the two functions are designed to be used together to reap the 
## caching benefit: 
##      my_matrix <- makeCacheMatrix( matrix( <values>, <dimensions>)
##      cacheSolve( my_matrix)
##      my_matrix$getinv()
##
## Some care must be taken to invoke cacheSolve if changing the matrix value via set, prior to
## invoking getinv.
##

## A factory function which yields an object encapsulating a square invertible matrix x,
## with the following methods:
##
##   get(), retrieves the current matrix; set( m) where m is assumed to be a square matrix;
##   getinv(), retrieves the inversion of x, or NULL if it has not been set.
##   setinv( inv) which assigns inv to the value returned by getinv(), but only if getinv() is NULL
##   set( new_x), assigns new_x to be the new matrix value, and ensures that if getinv() is the next
##   method invoked, NULL will be returned - see usage pattern at the top.
##
## If provided, x is assumed to be a square invertible matrix. Otherwise the default is a 1 by 1 empty matrix.
##
## Note that the matrix inversion computation is not itself provided, only its storage. See usage pattern above. 

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


## Yields the inverse of the x$get(), computing it only if necessary. 
## If computed, the result is cached, i.e. future invocations of  x$getinv() will return
## the same value without computation until the next invocation of  x$set.

cacheSolve <- function(x, ...) {

        ## Return current inverse if there is one
        cached <-  x$getinv()
        if( !is.null( cached))
            return( cached) 

        message( "Computing inverse");
        ## Cache the result and yield
        x$setinv( solve(x$get(), ...))
        x$getinv()
}
        
