## These functions cache the inverse of a matrix rather than computing it 
## repeatedly

## Creates a matrix-like object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {
    inv <<- inverse}
  getInverse <- function() {inv}
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Returns a matrix that is inverse of x

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
}
