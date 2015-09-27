## These two function takes a matrix x, compute its inversion
## with the cache method.

## This function create a list contained 4 function:
## set the matrix
## get the matrix
## set the inverse matrix
## get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function returns a matrix that is inverse of 'x'.
## First it check if inversed matrix was computed before, if it is,
## return the cached one, if not, compute it now.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
