## Put comments here that give an overall description of what your
## functions do
## Bo Liu

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##This function computes the inverse of the special "matrix"
##by makeCacheMatrix above. If the inverse has already been
##calculated (and the matrix has not changed), then cacheSolve should
##etrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  
}

