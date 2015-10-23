## 2015-10-22
## this will build a list for handling a matrix 
## as well as supporting cache-ing that matrix
## if it the cacheSolve function is called in 
## order to produce an inverse matrix

## this function takes an incoming matrix
## and creates a set and get function setting
## the appropriate functions, as well as the 
## functions for setting and getting the
## inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function will compute the inverse of a
## matrix using the solve(XXX) function, and
## will check that a matrix has not already been
## stored in the cache, and if not will compute
## it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data, because not yet computed")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
