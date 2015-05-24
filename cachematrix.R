## This is a script to create 2 functions
## The first one inverts a matrix and cache the result in memory
## The second one searches for the cache result

## This function creates the matrix and caches the result in memory

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmat <- function(inverse) m <<- inverse
  getinvmat <- function() m
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}

## This function searches for cached results and if not found calculates and returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getinvmat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinvmat(m)
  m
}
