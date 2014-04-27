## Below are two functions that are used to create a special object that stores 
## a matrix and cache's its inverse.
## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
##
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse of the matrix
##  get the value of the inverse of the matrix
##
##  Example:
##
##  mdat <- matrix(c(1,2, 3,2), nrow = 2, ncol = 2, byrow = TRUE)
##  mtrx <- makeCacheMatrix( mdat )
##  cacheSolve(mtrx)

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)

}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated and the matrix 
## has not changed, then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Check if the matrix that is the inverse of 'x' has been calculated and cached
  ## If m is not empty, then return matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached inverse matrix data")
    ## Return the matrix that is the inverse of 'x'
    return(m)
  }
  ## Otherwise, calculate and cache the matrix that is the inverse of 'x'
  ## and then return it
  
  ## Retrieve the matrix
  data <- x$get()
  
  ## Calculte the matrix that is the inverse of 'x'
  m <- solve(data)
  
  ## Store matrix that is the inverse of 'x' in cache
  x$setmatrix(m)
  
  ## Return the matrix that is the inverse of 'x'
  m
}