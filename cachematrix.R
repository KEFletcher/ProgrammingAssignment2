## This function makes a matrix object that can cache its inverse
## Modeled after makevector.R

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list (ste = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix
## Modeled after cachemean.R

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
