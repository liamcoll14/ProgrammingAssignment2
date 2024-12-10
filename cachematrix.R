## Set of functions to calculate and cache the inverse of a matrix

## List of functions to set and get value of matrix, then get and set its inverse
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



##  Return a matrix that is the inverse of 'x'
##If solution already in cache then function calls that value rather than recalculating
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ##Solve calcuates inverse of a square matrix
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
