## Put comments here that give an overall description of what your
## functions do

## Creates a 'special' matrix whose inverse can be cached.
makeCacheMatrix <- function(x = matrix()) {
  # The cached inverse is set to null because
  # we have just created the 'special' matrix.
  m <- NULL
  
  # Sets the 'special' matrix.
  set <- function(y = matrix()) {
    x <<- y
    # The cached inverse is set to null because we have changed
    # the 'special' matrix.
    m <<- NULL
  }
  
  # Returns the 'special' matrix.
  get <- function() {
    x
  }
  
  # Sets the inverse of the 'special' matrix.
  setInverse <- function(inverse) {
    m <<- inverse
  }
  
  # Returns the inverse of the 'special' matrix.
  getInverse <- function() {
    m
  }
  
  # A list containing the four functions is returned.
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Calculates the inverse of a 'special' matrix. If the inverse has already been calculated
## and the contents of the matrix have not changed, the inverse is not recalculated and the
## value returned is obtained from the cache.
cacheSolve <- function(x) {
  # The value of the inverse of the 'special' matrix is obtained.
  m <- x$getInverse()
  
  if (!is.null(m)) {
    message('Getting cached inverse.')
  } else {
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)    
  }
  # m, that now contains the inverse of the 'special matrix', is returned.
  m
}
