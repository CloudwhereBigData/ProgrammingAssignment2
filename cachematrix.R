## The functions makeCacheMatrix and cacheSolve are used for matrix inversion 
## and caching the results for future requests for better performance

## makeCacheMatrix - function will create a matrix object that can cache the inverse 
##                  for future requests.

makeCacheMatrix <- function(mInit = matrix()) {

  mInverse <- NULL
  
  ## set matrix in cache and clear inverse
  set <- function(y) {
    mInit <<- y
    mInverse <<- NULL
  }
  ## get matrix from cache
  get <- function() mInit
  
  ## set matrix inverse into cache
  setInverse <- function(x) mInverse <<- x
  
  ## get matrix inverse from cache
  getInverse <- function() mInverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)   
}

## cacheSolve function takes the inverse of a square Matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## see if matrix has already been inversed, if yes get cached result
  m <- x$getInverse()
  if(!is.null(m)) {
    return(m)
  }
  ## matrix inverse not in cache, create inverse, store in cache for future requests
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m  
}
