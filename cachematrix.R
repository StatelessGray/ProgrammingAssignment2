##Coursera Week 3 Programming assignment: makeCacheMatrix sets a matrix as input and the value of the Matrix. Gets the value of the matrix, sets the inverse Matrix
## and gets the inverse Matrix. 

## Creates a special matrix object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) invMatrix <<- inverse
  getInverse <- function() invMatrix
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of the matrix returned above.

cacheSolve <- function(x, ...) {
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {
    message("Getting Cached Data")
    return(invMatrix)
  }
  MatrixData <- x$getMatrix()
  invMatrix <- solve(MatrixData, ...)
  x$setInverse(invMatrix)
  return(invMatrix)
        ## Return a matrix that is the inverse of 'x'
}
