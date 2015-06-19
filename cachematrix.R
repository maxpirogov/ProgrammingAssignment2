## Function below stores the intrnal functions for retaining the matrix and the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setMatrix <- function(new) {                           ## set the new matrix
    x <<- new
    inverse <<- NULL                                     ## clear the value of the Inversed matrix   
  }
  getMatrix <- function() x                              ## returns the value of matrix
  setInverse <- function(inverse) inverse <<- inverse    ## set the value of the inverse of the matrix
  getInverse <- function() inverse                       ## prints the stored value of the inverse of the matrix    
  list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse) ## stores the list of internal functiones
}


## Function below checks the presence of the inverse of the function and calculates the inverse of the matrix if needed.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()                           ## retrive the inverse of the matrix from above function
  if(!is.null(inverse)) {                             ## checks if the inverse was already calculated previously
    message("getting cached data.")
    return(inverse)
  }
  data <- x$getMatrix()                               ## retrive the matrix      
  inverse <- solve(data)                              ## calculates the 
  x$setInverse(inverse)                               ## stores the inverse of the matrix 
  inverse                                             ## prints the inverse of the matrix
}

