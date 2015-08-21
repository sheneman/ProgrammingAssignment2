## Cached Matrix Functions
##
## Programming in R - Programming Assignment #2
##
## This file contains two functions which cache the inverse of a matrix.
##

## makeCacheMatrix() is a function which creates a special matrix object
## that is able to store a cache of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  
  inverse_matrix <- NULL
  
  set <- function(y) {
    message("in makeCacheMatrix.set() --")
    x <<- y
    inverse_matrix <<- NULL
  } 
  
  get <- function() {
    message("in makeCacheMatrix.get() --")
    x
  }
  
  setinverse <- function(i) {
    message("in makeCacheMatrix.setinverse() --")
    inverse_matrix <<- i
  }
  
  getinverse <- function() {
    message("in makeCacheMatrix.getinverse() -")
    inverse_matrix
  }
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() is a function which computes the inverse of the special
## matrix object created by makeCacheMatrix().   If the inverse of the 
## marix has already been computed, then it retrieves the pre-computed 
## from the cache instead of recomputing it.
## 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  inverse_matrix <- x$getinverse()
  if(!is.null(inverse_matrix)) {
    message("getting cached inverse matrix")
    return(inverse_matrix)
  }

  matrix <- x$get()
  message("ACTUALLY INVERTING THE MATRIX")
  inverse_matrix <- solve(matrix, ...)
  x$setinverse(inverse_matrix)
  
  inverse_matrix
}

test_it <- function() {
  message("Testing the Matrix Inverse Caching Functions")
  
  ## Create an initial matrix that I pre-determined to be invertable
  t <- c(1,2,3,0,1,4,5,6,0)
  x <- matrix(t, nrow=3, ncol=3)
  
  matrix <- makeCacheMatrix()
  matrix$set(x)
  
  cacheSolve(matrix)
  a <- matrix$getinverse()
  print(a)
  
  cacheSolve(matrix)
  b <- matrix$getinverse()
  print(b)
        
  cacheSolve(matrix)
  c <- matrix$getinverse()
  print(c)
  
}
