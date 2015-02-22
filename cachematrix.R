## Put comments here that give an overall description of what your
## functions do
##pair of functions that cache the inverse of a matrix.
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
##            If the inverse has already been calculated (and the matrix has not changed), 
##            then the cachesolve will retrieve the inverse from the cache.
##Computing the inverse of a square matrix can be done with the solve function in R. 
##For example, if X is a square invertible matrix, then solve(X) returns its inverse.
#########################################################################################
##
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve: calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache, 
## it is created and it's inverted matrix is stored in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<- solve(data, ...)
  x$setinv(m)
  m
  
}
