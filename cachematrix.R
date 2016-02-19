## Matrix inversion is usually a costly computation so there is a benefit to cache the inverse of a matrix rather than computing it repeatedly 
## Functions below implement caching of comuted inverse matrix


## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inversed <- NULL
    set <- function(y) {
      x <<- y
      inversed <<- NULL
    }
    get <- function() x
    setSolve <- function(inv) inversed <<- inv
    getSolve <- function() inversed
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then returns inverse from the cache

cacheSolve <- function(x, ...) {
    inversed <- x$getSolve()
    if(!is.null(inversed)) {
      message("getting cached data")
      return(inversed)
    }
    data <- x$get()
    inversed <- solve(data)
    x$setSolve(inversed)
    inversed
}
