## A pair of functions are written to cache the inverse of a matrix rather than 
## computing it repeatedly

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initializing the inverse property
  i <- NULL
  
  ## Method to set the matrix
  set <- function( matrix ) {
    x <<- matrix
    i <<- NULL
  }
  
  ## Method to get the matrix
  get <- function() {
    ## Return the matrix
    x
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## Return a list of all the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  y <- x$getInverse()
  
  ## Return the inverse if its already set
  if( !is.null(y) ) {
    message("getting cached data")
    return(y)
  }
  
  ## Get the matrix from the object
  data <- x$get()
  
  ## Calculate the inverse with matrix multiplication
  y <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(y)
  
  ## Return the matrix
  y
}
