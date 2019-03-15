## These functions are designed to calculate the inverse of an matrix, but to use the cache value if it has already been calculated

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y){
    	x <<- y
    	inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(cache_matrix) inverse <<- cache_matrix
	getinverse <- function() inverse
	list(set = set, get = get, setinverse = setinverse, getinverse =	getinverse)
}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated then the cachesolve should retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse      
}
