## cacheMatrix.R
## Forked from Coursera's R Programming class: rdpeng/ProgrammingAssignment2
## Current Author: Elan Grossman github.com/egrossman



## makeCacheMatrix
## Given a square invertible matrix, returns a list of functions for
## getting and setting the matrix object and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  ## set the inverse to null since this is a new matrix
  inv <- NULL
  
  ## Initiates the matrix by assigning it to a value and setting the inverse to null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #returns x
  get <- function() x
  ## sets the inverse by passing thr solve function
  setinverse <- function(solve) inv <<- solve
  #returns the inverse of the matrix
  getinverse <- function() inv
  
  ## Return List
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Given a square invertible matrix, returns its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  inv <- x$getinverse()
  ## If inverse is not null for the given matrix, then returned it's cached data
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Else, get the data and solve for the inverse 
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
