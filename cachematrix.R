makeCacheMatrix <- function(x = matrix()) {
  
  ## x is an invertible matrix
  ## returns a list of functions to:
  ##    1. set the value of the matrix
  ##    2. get the value of the matrix
  ##    3. set the value of inverse of the matrix
  ##    4. get the value of inverse of the matrix
  
  inv <- NULL
  set <- function(y) {
    
    x <<- y
    inv <<- NULL
    
  }
    
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}

cacheSolve <- function(x, ...) {
  
  ## x is the output of makeCacheMatrix
  ## checks if inverse is in cache and pulls the value out
  ## if not, calculates the inverse and enters it into makeCacheMatrix
  
  inv <- x$getinv
  
  if (!is.null(inv)){
    
    # if inverse has been calculated, skips computation and takes value from makeCacheMatrix
    
    message("Getting cached data")
    return(inv)
    
  }
  
  ## otherwise calculates the inverse
  
  matrixData <- x$get
  inv <- solve(matrixData, ...)
  
  ## sets value of inverse in makeCacheData
  
  x$setinv(inv)
  return(inv)
  
}
