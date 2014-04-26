
## Matrix inversion is usually a costly computation and their 
## may be some benefit to caching the inverse of a matrix rather 
## than compute it repeatedly. The two functions below are designed to
## compute the inverse of a matrix by using a cache value system.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL # initialize the inverse matrix with 'NULL'
  set <- function(y) { 
    x <<- y # set a new value to the 'x' matrix
    s <<- NULL # if 'x' changes the value of 's' is removed
  }
  get <- function() x # return 'x' matrix value
  setinv <- function(inv) s <<- inv # assign the value of inverse matrix to 's'
  getinv <- function() s # return inverse matrix value
  
  # return the set, get, setinv and getinv functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Return a matrix that is the inverse of the special matrix 'x'.
## We assume that the square matrix 'x' is always invertible.

cacheSolve <- function(x, ...) {
  
  s <- x$getinv()
  
  # if the inverse has already been calculated, a cache value is found and returned
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  # else the value of x is get and the inverse is computed
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s) # assign inverse matrix value to 's' variable
  s # return the value of inverse matrix
}
