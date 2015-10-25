## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # these are all functions. Do not think of them as variables
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inv) m <<- Inv
  getInverse <- function() m
  
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function
## 1. This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## 2. If the inverse has already been calculated (and the matrix has not changed), 
##    then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(a=data,...)
  x$setInverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
