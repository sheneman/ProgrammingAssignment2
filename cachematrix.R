## Cached Matrix Functions
##
## Programming in R - Programming Assignment #2
## Luke Sheneman
## sheneman@uidaho.edu
##
## This file contains two functions which cache the inverse of a matrix.
##

## makeCacheMatrix() -- 
## 
## This function creates a special matrix object that defines a set of sub-functions that
## is able to store a matrix and a cache of the inverse of that matrix
makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize the inverse matrix if this function is called directly
  inverse_matrix <- NULL
  
  ## set() is a function that sets the value of the actual initial (un-inverted) matrix
  set <- function(y) {
    
    ## message("in makeCacheMatrix.set() --")   ## some debugging information
    
    ## set the uninverted matrix (x) in the parent namesepace equal to the given matrix (y)
    x <<- y  
    
    ## since we are setting a new initial matrix, lets initialize the inverse_matrix
    inverse_matrix <<- NULL   
  } 
  
  
  ## get() is a function that simply returns the basic uninverted matrix
  get <- function() {
    ## message("in makeCacheMatrix.get() --")   ## some debugging information
    x
  }
  
  ## setinverse() is a function that sets the cached inverted matrix
  setinverse <- function(i) {
    ## message("in makeCacheMatrix.setinverse() --")  ## some debugging information
    inverse_matrix <<- i
  }
  
  ## getinverse() is a function that returns the cached inverted matrix
  getinverse <- function() {
    ## message("in makeCacheMatrix.getinverse() -")  ## some debugging information
    inverse_matrix
  }
  
  ## return a list of the sub-functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




## cacheSolve() is a function which computes the inverse of the matrix within
## the special matrix object created by makeCacheMatrix().   If the inverse of 
## the matrix has already been computed, then it retrieves the pre-computed 
## from the cache instead of recomputing it.
## 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  ## retreive the cached inverse matrix from the special matrix object
  ## if it has already been computed, just return the cached computed matrix
  inverse_matrix <- x$getinverse()
  if(!is.null(inverse_matrix)) {
    message("GETTING CACHED INVERTED MATRIX")
    return(inverse_matrix)
  }

  ## if we are here, there is no cached inverted matrix
  ## retreive the uninverted matrix from the special matrix object
  ## and then invert it using the solve() function and set the 
  ## value of the cached inverted matrix to the solved inverted matrix
  matrix <- x$get()
  message("ACTUALLY COMPUTING THE INVERTED THE MATRIX")
  inverse_matrix <- solve(matrix, ...)
  x$setinverse(inverse_matrix)
  
  ## return the inverted matrix
  return(inverse_matrix)
}



## test_it()
##
## This function is a little test routine to make sure that everything is working.
## It does the following:
##  
##   1.   Creates a small initial matrix that is known to be invertable
##   2.   Initializes the special cached matrix object
##   3.   Sets the value of the uninverted matrix in the special cached matrix object
##   4.   calls cacheSolve() on the uninverted matrix object THREE times:
##      a) - the first time cacheSolve() is called, cacheSolve() must compute the inverted matrix
##      b) - the subsequent cacheSolve() calls simply return the cached inverted matrix
##    
test_it <- function() {
  message("Testing the Matrix Inverse Caching Functions")
  
  ## Create an initial matrix that I pre-determined to be invertable
  t <- c(1,2,3,0,1,4,5,6,0)
  x <- matrix(t, nrow=3, ncol=3)
  
  message("THE INITIAL, UNINVERTED MATRIX:")
  print(x)
  
  matrix <- makeCacheMatrix()
  matrix$set(x)
  
  message("Calling makeCacheMatrix() for the FIRST time")
  cacheSolve(matrix)
  a <- matrix$getinverse()
  print(a)
  
  message("Calling makeCacheMatrix() for the SECOND time")
  cacheSolve(matrix)
  b <- matrix$getinverse()
  print(b)
   
  message("Calling makeCacheMatrix() for the THIRD time")     
  cacheSolve(matrix)
  c <- matrix$getinverse()
  print(c)
  
}
