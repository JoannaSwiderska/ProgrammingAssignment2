## R Programming
## Assignment 2: 
## Caching the Inverse of a Matrix

## Below function creates a special "matrix" object that can cache its inverse.
## Assumption is made that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}


## Example of usage:
## > mt<-matrix(runif(9),nrow=3,ncol=3)
## > mtc <- makeCacheMatrix(mt)
## > cacheSolve(mtc)
##           [,1]      [,2]      [,3]
## [1,]  4.656741 -2.729661  0.589171
## [2,] -9.939629  4.606468  2.266674
## [3,] -4.976221  4.479396 -1.133469
## > cacheSolve(mtc)
## getting cached data
##           [,1]      [,2]      [,3]
## [1,]  4.656741 -2.729661  0.589171
## [2,] -9.939629  4.606468  2.266674
## [3,] -4.976221  4.479396 -1.133469
