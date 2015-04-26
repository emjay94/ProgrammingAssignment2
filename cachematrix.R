## These functions facilitate matrix inversion with caching ability in order to
## prevent resource consuming inversion process due to repeated computation.


## This function creates a matrix object that can cache its inverse.
## set() and get() subfunctions are also provided.

makeCacheMatrix <- function(x = matrix()) {
  
    inversemat <- NULL
    set <- function(y) {
      x <<- y
      inversemat <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inversemat <<- inverse
    getinverse <- function() inversemat
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)  
}


## This function computes the inverse of the matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated,
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    mat <- x$get()
    inverse <- solve(mat, ...)
    x$setinverse(inverse)
    inverse
  ## Return a matrix that is the inverse of 'x'
}
