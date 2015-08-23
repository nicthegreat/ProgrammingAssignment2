## Writing two functions in order to cache the value of the inverse of a matrix
## such that when we need it again, we can look up the cache rather than
## recompute it, which would be time consuming

## The first function creates a special matrix called makeCacheMatrix
## which is a matrix designed to set a null matrix, get the matrix value,
## set the value of the inverse and then get the value of the inverse

makeCacheMatrix <- function(xMatrix = matrix()) {
  
  inverse <- NULL ## creates a placeholder for future "inverse" values
  
  ## sets non-inverted matrix x to a new matrix, y, a resets inverse to NULL
  set <- function(y){
    xMatrix <<- yMatrix  ## <<- operator is used so search is done in parent environment, before global environment
    inverse <<- NULL
  }
  
  get <- function() xMatrix  ## gets the non-inverted input matrix
  
  setInverse <- function(inv) inverse <<- inv ## sets the inverse matrix
  
  getInverse <- function() inverse ## gets the inverse matrix
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The second function is used to calculate the inverse of the matrix
## created with the first function, but first checks to see if the inverse
## has already been calculated - and if so - gets the inverse and skips the
## computation. Otherwise, it calculates the inverse and sets the value
## in the setInverse function

cacheSolve <- function(xMatrix, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- xMatrix$getInverse() ## get the inverse matrix of xMatrix
  
  ## if inverse has been calculated, return the inverse
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  matrixData <- xMatrix$get() ## if not, get xMatrix
  inverse <- solve(matrixData)  ## calculate inverse of matrixData
  ## try substituting for %*% matrixData
  
  xMatrix$setInverse(inverse) ## set the inverse
  inverse ## return inverse matrix
  
}