## The following functions implement caching the inverse of a matrix
## The principal idea behind this task is to save time needed for computing the inverse of a matrix 
## whenever that's possible
## In case the inverse of a given matrix is already computed and the matrix itslef was not changed, 
## a cached inverse is returned,
## otherwise the inverse is firstly calculated, then cached and then returned

## makeCacheMatrix function creates a special matrix object that can cache its inverse
## This matrix is actually a list with functions to set/get matrix (setMatrix, getMatrix) 
## and set/get cached inverse of a matrix (setInvMatrix, getInvMatrix)
## The input argument for this function is an invertible matrix object

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  
  setMatrix <- function(y)
  {
    x <<- y
    invMat <<- NULL
  }
  
  getMatrix <- function() x
  
  setInvMatrix <- function(iMat)
  {
    invMat <<- iMat
  }
  
  getInvMatrix <- function() invMat
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## cacheSolve is a function whose responisibility is to return inverse of a matrix
## the argument x is a cache matrix object made with makeCacheMatrix function
## the function first checks if there is cached inverse of a matrix
## in case there is, the function returns cached inverse, otherwise it calculates the inverse of the matrix
## caches it and then returns it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  Inv <- x$getInvMatrix()
  
  if(!is.null(Inv))
  {
    message("getting cached inverse of the matrix")
    return(Inv)
  }
  
  mat <- x$getMatrix()
  Inv <- solve(mat)
  x$setInvMatrix(Inv)
  Inv
}
