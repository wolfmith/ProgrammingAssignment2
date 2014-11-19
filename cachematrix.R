## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. 
## The following pair of functions cache the inverse of a matrix. 
## It's assumed the matrix is always invertible.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {       ## input x will be a matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {x}                           ## this function returns the value of the original matrix
  setinverse <- function(solve) {m <<- solve}     ## this is called by cacheSolve() during the first cacheSolve() when cache value is NULL
  getinverse <- function() {m}                    ## this will return the cached value to cacheSolve() on subsequent accesses
  list(set = set,
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)                   ## Creates a list with the internal functions (methods)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {                  ## Return a matrix that is the inverse of 'x'
        
  m <- x$getinverse()                             ## Get's the function inverse from cache (if none exists, will return NULL)
    if(!is.null(m)) {                             ## Checks if the function inverse exists in cache and returns if TRUE
    # message("getting cached data")              ## Unlock this if you want to get confirmation the function is getting the result from cache
    return(m)
  }
  data <- x$get()                                 ## If no inverse matrix in cache, will solve it and store in cache
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
