## makeCacheMatrix caches inverse of a matrix
## cacheSolve computes inverse of a matrix if it isn't already cached

## creates a special vector (as matrix) and caches its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverseX <- NULL
  set <- function(y) {
    x <<- y
    inverseX <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverseX <<- inv
  getInverse <- function() inverseX
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## computes the inverse of a matrix (x) if it isn't already cached by makeCacheMatrix function
cacheSolve <- function(x, ...) {
  ## get cached inverse of x
  inverseX <- x$getInverse()
  ## check if inverse of x has value, if yes return it, else compute inverse of x
  if (!is.null(inverseX)) {
    message("Getting cached data.")
    return(inverseX)
  }
  ## inverseX is null, compute its inverse, set computation to cache and return computation
  data <- x$get()
  inverseX <- solve(data, ...)
  x$setInverse(inverseX)
  inverseX
}

