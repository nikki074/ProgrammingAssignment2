## makeCacheMatrix and cacheSolve functions - to cache
## the inverse of the matrix

## input - a matrix, output - a list of functions that can set,get data
## and set inverse and get cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y){
    x <<- y
    inv <<- NULL
  }

  get <- function() {
    x
  }

  setinverse <- function(inverse){
    inv <<- inverse
  }

  getinverse <- function(){
    inv
  }

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## input - output of makeCacheMatrix, output - either the cached
## or computed inverse of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mtrx <- x$getinverse()
  if(!is.null(mtrx)) {
    message("getting cached data")
    return(mtrx)
  }
  
  data <- x$get()
  mtrx <- solve(data, ...)
  x$setinverse(mtrx)
  
  mtrx
}
