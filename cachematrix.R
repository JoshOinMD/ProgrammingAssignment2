## Matrix inversion is usually a costly computation 
## There may be some benefit to caching the inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
    Minv <- NULL
    set <- function(y) {
      x <<- y
      Minv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) Minv <<- inverse
    getinverse <- function() Minv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
 

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  Minv <- x$getinverse()
    
    if(!is.null(Minv)) {
      message("getting cached data")
      return(Minv)
    }
    data <- x$get()
    Minv <- solve(data, ...)
    x$setinverse(Minv)
    Minv
 
  
  
        ## Return a matrix that is the inverse of 'x'
}
