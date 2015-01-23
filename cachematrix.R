## These functions cache the inverse of an invertible matrix
## so that if the contents of the matrix have not changed the
## inverse does not have to be recomputed. 

## The first function creates a list of functions to:
## 1. set the vaue of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) i <<- solve
      getsolve <- function() i
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}
      
## The second function calculates the inverse, but it first checks
## via getsolve to see if it has already been calculated. If so, it
## gets the cached value and skips the calculation. If not, it calculates
## the inverse and caches it via the setsolve function.

cachesolve <- function(x, ...) {
      i <- x$getsolve()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setsolve(i)
      i
}