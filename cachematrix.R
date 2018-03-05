## This program allows for the inversion and caching of matrices.

## Setting up the caching process - This component initializes x and m, defines all the
## functions in makeCacheMatrix, and it then creates a list to be used by the subsequent
## code cacheSolve

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


## This section of code confirms whether the inverse of the matrix is already
## cached.  If it is, then the cached matrix is provided.  If not, then the inverse
## of the matrix is calculated.

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
  
