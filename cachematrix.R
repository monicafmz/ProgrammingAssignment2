## Put comments here that give an overall description of what your
## functions do

## This function works almost the same like makeVector. Only variable are not "mean and m" but "inverse and inv"
## we have a set & get function for the matrix, and a set & get for its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function works almost the same like cachemean, instead of mean it calculates the inverse of 
## an invertible matrix, if not done already.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}