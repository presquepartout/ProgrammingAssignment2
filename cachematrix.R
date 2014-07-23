## This code provides a mechanism for caching inverted matrices.
## It contains two functions: makeCacheMatrix, which creates the 
## cache, and cacheSolve, which returns inverted matrices. 
## If no inverted matrix is cached, cacheSolve inverts its input.

## The following function creates a cache for inverted matrices.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
              x <<- y
              m <<- NULL
      }
      get <- function() x
      setSolve <- function(solve) m <<- solve
      getSolve <- function() m
      list(set = set, get = get,
           setSolve = setSolve,
           getSolve = getSolve)

}


## The following function returns a matrix that  is the
## inverse of its argument. If the inverse is cached, it
## retrieves it from the cache; otherwise, it inverts. 

cacheSolve <- function(x, ...) {
        m <- x$getSolve()
        if(!is.null(m)) {
              message("getting cached data")
              return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}

## Testing this code: 
## > source("./cachematrix.R")
## > myMatrix <- matrix(1:4, nrow=2, ncol=2)
## > x <- makeCacheMatrix(myMatrix)
## > cacheSolve(x)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## (Repeat call to cacheSolve to show retrieval of cache version)
## > cacheSolve(x)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
