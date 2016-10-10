## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Req : makeCacheMatrix: This function creates a special "matrix"
##  object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## create a blank object
  inverse <- NULL
  
  #
  set <- function(y) {
    x <<- y;
    inverse <<- NULL;
  }
  
  #
  get <- function() return(x);
  
  setinv <- function(inv) inverse <<- inv;
  
  getinv <- function() return(inverse);
  
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## Write a short comment describing this function

## requirement : cacheSolve: This function computes 
## the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  
  ## check it inserver is not nulll
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  
  ## if null then inverse and return
  data <- x$get()
  
  invserse <- solve(data, ...)
  
  x$setinv(inverse)
  
  return(inverse)
}
