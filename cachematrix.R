## Put comments here that give an overall description of what your
## functions do

## ----
## This function sets up a matrix that can store its inverse in cache
## First create a a square matrix with the following command
## theMatrix <- matrix(c(1:4), nrow=2, ncol=2)  
## 
## Then cache the matrix using the makeCacheMatrix function with the following command
## cachetheMatrix <- makeCacheMatrix(theMatrix) 
##
## Then return the inverse using the following command
## cacheSolve(cachetheMatrix) 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function returns the inverse of cachetheMatrix created above, using the solve() function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
