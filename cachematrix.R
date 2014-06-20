## This script creates a cached value of inverse of a matrix
## which means that the inverse of a matrix will only be
## calculated if the value doesn't already exist in the cache

## This function creates a vector which is a list of functions
## to set the value of the matrix, get the value of the matrix,
## set the value of the inverse and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  xinverse <- NULL
  set <- function(y){
    x <<- y
    xinverse <<- NULL
  }
  get <- function()x
  setinverse <- function(inverted){
    xinverse <<- inverted
  }
  getinverse <- function()xinverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function checks if the value of the inverse of the matrix
## already exists. If the inverse doesn't exist then the inverse
## is calculated and set in the cache for future use.

cacheSolve <- function(x, ...) {
  xinverse <- x$getinverse()
  if(!is.null(xinverse)){
    message("Getting cached matrix inverse")
    return(xinverse)
  }
  data <- x$get()
  xinverse <- solve(data)
  x$setinverse(xinverse)
  xinverse
}
