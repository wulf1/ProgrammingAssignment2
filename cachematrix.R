## Matrix inversion may be computationally costly sometimes. Therefore the following functions are able to
## compute the inverse of a matrix and additionally cache the results.

## This function is able to create a matrix object which caches its inverse

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


## This function is able to calculate the inverse of the matrix

cacheSolve <- function(x, ...) {  
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
        ## Return a matrix that is the inverse of 'x'
}
