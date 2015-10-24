## The combination of functions below will cache 
## the inverse of a matrix, which can be a 
##costly computation.

## This function creates a special "matrix
## that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(Y) {
    X <<- Y
    i <- NULL
  }
  get <- function() X
  setinverse <- function(solve) i <<-solve
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the invesere of the special
## "matrix" returned by makeCacheMatrix. If the 
## inverse has already been calculated (and the 
## matrix not changed), then cacheSolve should
## retrieve the inverse from the cache. Assumption
## was made that the matrix supplied is always 
## invertible. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- X$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- X$get()
  i <- solve(data, ...)
  X$setinverse(i)
  i
}
