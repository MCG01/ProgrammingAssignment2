
## R Programming - Assignment 2


## Put comments here that give an overall description of what your
## functions do

## ----
## makeCacheMatrix
## This function sets up a matrix that can store its inverse in cache
## First create a a square matrix with the following command
##
##          theMatrix <- matrix(c(1:4), nrow=2, ncol=2)  
## 
## Then cache the matrix using the makeCacheMatrix function with the following command
## 
##          cachetheMatrix <- makeCacheMatrix(theMatrix) 
##
## It creates the cache matrix, then gets the values from theMatrix, then sets the inverse of theMatrix 
## and then gets the inverse of theMatrix.

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

## ----
## cacheSolve
## This function returns the inverse of cachetheMatrix object. It does this by first checking
## if the inverse is stored (getInverse). If it is there it returns it, if it's not (null) it calculates it and then
## sets it to cache (setInverse). This logic save processing time by only calculating the inverse when required.

## Return the inverse using the following command
##
##            cacheSolve(cachetheMatrix) 
##

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
