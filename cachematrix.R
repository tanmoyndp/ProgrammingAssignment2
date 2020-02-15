## Functions that cache the inverse of a matrix.
## This function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  set_inv <- function(solveMatrix) inverse <<- solveMatrix
  get_inv <- function() inverse
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## This function calculates the inverse of the matrix returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$get_inv()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$set_inv(inverse)
  inverse      
}