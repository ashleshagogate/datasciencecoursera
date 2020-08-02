## makeCacheMatrix is a function that creates a special matrix which is actually a list. 
## This list contains a function to set and get the value of the matrix as well as set and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y) {
    x <<- y
    j <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) j <<- inverse
  getinverse <- function() j
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve function computes the inverse of the special matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  j <- x$getinverse()
  if (!is.null(j)) {
    message("getting cached data")
    return(j)
  }
  data <- x$get()
  j <- solve(data, ...)
  x$setinverse(j)
  j
}

