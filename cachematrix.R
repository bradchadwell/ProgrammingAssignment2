## R Programming Assignment 2
## Brad Chadwell
## September 15, 2015
##
## The two functions below are able to cache potentially time-consuming
## computations, so that a calculation--specifically in this case,
## computing the inverse of a matrix--does not have to be repeated.
## Instead, the functions calculate the matrix inverse the first time through,
## and use the cached matrix inverse on subsequent calle, first checking
## to confirm the matrix has not changed.
##
## The program demonstrates how to take advantage of the scoping rules of
## the R language and how they can be manipulated to preserve state inside
## of an R object.
##
## The first function, makeCacheMatrix, creates a special "matrix" object
## that can cache its inverse. The second function, cacheSolve, computes the
## inverse of the cached matrix, stores it in the cache, and on subsequent
## calls, retrieves it from the cache instead of computing it.
##
## Usage:
## 1. Assign a "matrix" to a variable by passing a matrix object argument
##    to makeCacheMatrix().
## 2. Call cacheSolve() with the "matrix" variable assigned above as an
##    argument to return the calculated (or cached) matrix inverse.
##
## Caveats:
## 1. This program assumes that all matrices passed to it can be inverted.
## 2. Always pass cacheSolve a "matrix" variable assigned by makeCacheMatrix.

## makeCacheMatrix:
## This function creates a special "matrix" object that can cache its inverse.
## The "matrix" is really a list containing a function to:
##         1. set the value of the matrix
##         2. get the value of the matrix
##         3. set the value of the inverse
##         4. get the value of the inverse
## This list is used as input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        # argument x is a square invertable matrix

        # initialize inv as null
        inv <- NULL
        # 1. set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # 2. get the value of the matrix
        get <- function() x
        # 3. set the value of the inverse
        setinverse <- function(solve) inv <<- solve
        # 4. get the value of the inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve:
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix() above. If the inverse has already been calculated
## (and the matrix has not changed), then this function retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## argument 'x' is a "matrix" established by makeCacheMatrix()
        ## Return a matrix that is the inverse of 'x'

        # Check for a cached inverse
        inv <- x$getinverse()
        if(!is.null(inv)) {
                # The "set" action in makeCacheMatrix clears the cached
                # inverse when matrix is changed (set);
                # So, if inv exists, it is current.
                message("getting cached data")
                # Return the cached inverse (and exit function)
                return(inv)
        }
        # Inverse not yet cached, so retrieve matrix and compute inverse.
        data <- x$get()
        inv <- solve(data, ...)
        # Cache the computed inverse and return the computed inverse
        x$setinverse(inv)
        inv
}