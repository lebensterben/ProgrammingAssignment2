## This script computes the inverse matrix of a special matrix cached by makeCacheMatrix function
## Essentially makeCacheMatrix is an constructor, or an enclosure, which creates an object with
## two properties, the special matrix and its inverse, and with two four methods, which are the IO
## interface of this object


## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of four functions
## foo$set(bar) updates the special "matrix", which is locally cached in variable x
## foo$get returns the current special "matrix"
## foo$setInverse(bar) store the inverse matrix in local cache named as i
## foo$getInverse tries to retrieve the inverse matrix from the local cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # initialise variable i, the inverse matrix of x
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
