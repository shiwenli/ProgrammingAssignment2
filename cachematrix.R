## Computing the inverse of a square matrix
## can be done with the solve function in R.
## For example, if X is a square invertible matrix,
## then solve(X) returns its inverse.

## This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) n <<- solve
  getsolve <- function() n
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  n <- x$getsolve()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setsolve(n)
  n
  ## Return a matrix that is the inverse of 'x'
}
