## This script calculates and caches the inverse of a matrix


## Creates a special 'matrix' object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
        getsolve = getsolve)
}


## Computes the inverse of the matrix returned by makeCacheMatrix
## NOTE: assumes the matrix supplied is always invertible
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
        
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

mat <- makeCacheMatrix(matrix(c(1,2,3,4), nrow = 2))
print(cacheSolve(mat))
