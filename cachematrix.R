## -- Put comments here that give an overall description of what your
## -- functions do
## 
## This file contains the R code for Programming Assignment 2 for the 
## R programming course on Coursera. The assignment involves writing R functions
## that cache the inversion of a matrix. Matrix inversion could potentially be 
## a time-consuming operation for larger dimensional matrices and caching the inverse 
## could yield potential benefits instead of recomputing the inverse each time.
## This is done by using the scoping rules in R to preserve state inside an R object


##  -- Write a short comment describing this function --
## 'makeCacheMatrix' : function that creates a special matrix object
## Input  : a square invertible matrix X
## Output : a list of functions to set matrix value, get matrix value, 
##          set matrix inverse value, get matrix inverse value

makeCacheMatrix <- function(x = matrix()) {
    ## x_inv : matrix inverse of x, initialize to NULL
    x_inv <- NULL
    
    ## setter for matrix x : set x to value 'mtrx1'; initialize x_inv to null
    set <- function(mtrx1) {
        x <<- mtrx1
        x_inv <<- NULL
    }
    
    ## getter for matrix x : returns matrix x
    get <- function() x
    
    ## setter for inverse matrix of x : set x_inv to value 'invX'
    setInverseX <- function(invX) x_inv <<- invX
    
    ## getter for inverse matrix of x : returns x_inv
    getInverseX <- function() x_inv
    
    # return a list containing functions for setting and getting x and x_inv.
    list(set = set, get = get, setInverseX = setInverseX, getInverseX = getInverseX)
}


## -- Write a short comment describing this function --
## 'cacheSolve' : function to cache inverse of input matrix, if not already cached
## Input  : a square invertible matrix x
## Output : inverse of input matrix 
cacheSolve <- function(x, ...) {
    ## get value of x_inv using the special matrix object from makeCacheMatrix
    x_inv <- x$getInverseX()
    if (!is.null(x_inv)) {
        ## x_inv is not null and has been cached, print message and return cached inverse
        message("Getting cached inverse of x")
        return(x_inv)
    }
    
    ## x_inv from special matrix object was null (no cached data),
    ## get the matrix itself from special matrix; compute inverse; 
    data <- x$get()
    x_inv <- solve(data)
    ## cache inverse of x in the special matrix by using setInverseX
    x$setInverseX(x_inv)
    ## Return a matrix that is the inverse of 'x'
    x_inv
}
