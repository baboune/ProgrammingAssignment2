## Put comments here that give an overall description of what your
## functions do

##  makeCacheMatrix is a function with the following properties:
## 1. it takes an argument x of matrix
## 2. it returns a list with 4 objects: set, get, setinverse and getinverse
## In other words, it converts a matrix into a special matrix enhanced with functions.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## CacheSolve expects an argument of type makeCacheMatrix (i.e. a special 
## matrix) and will return its inverse.  The input matrix is expected to be
## be invertible.
## If the matrix has already been inverted previously, then no computation is
## performed as the matrix inverse is returned from a cache.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
