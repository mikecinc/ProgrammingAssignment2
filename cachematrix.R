## These functions allow the user to calculate the inverse of a matrix
## and cache/store it for future use rather than re-calculating it each time

## Create a list of functions that will both set and get the values of 
## the input matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {

  this_inverse_matrix <- NULL
  ## defining the "set" function. once the makeCacheMatrix is called for
  ## for a matrix (x), this function can be used to pass in a different
  ## matrix input parameter, with the inverse being set back to null
  set <- function(y) {
    x <<- y
    this_inverse_matrix <<- NULL
  }
  ## defining the "get" function
  get <- function() x
  ## defining the "setinverse" function
  setinverse <- function(inverse) this_inverse_matrix <<- inverse
  ## defining the "getinverse" function
  getinverse <- function() this_inverse_matrix
  
  ## defining the list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}

## This function will first check if the inverse of the matrix has already
## been calculated and stored. If so, it will retrieve the inverse. If not,
## it will calculate the inverse and then store it so that future calls
## will not need to re-calculate.

cacheSolve <- function(x, ...) {
  ##check to see if the inverse matrix has already been calculated
  ##and stored
  this_inverse_matrix <- x$getinverse()
  ##the inverse has been found, so return it along with a message
  if(!is.null(this_inverse_matrix)) {
    message("getting cached data")
    return(this_inverse_matrix)
  }
  data <- x$get()
  this_inverse_matrix <- solve(data, ...)
  x$setinverse(this_inverse_matrix)
  this_inverse_matrix
}
