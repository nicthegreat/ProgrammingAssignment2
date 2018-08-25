## Writing two functions in order to cache the value of the inverse of a matrix
## such that when we need it again, we can look up the cache rather than
## recompute it, which would be time consuming

## The first function creates a special matrix called makeCacheMatrix
## which is a matrix designed to set a null matrix, get the matrix value,
## set the value of the inverse and then get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL    #sets inv to null, placeholder for future values
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }      #defines a function to set matrix x to a new matrix y, and resets mean to null
  get <- function() x      #returns the matrix x
  setinv <- function(inverse) inv <<- inverse #sets the inverse, inv, to the "inverse"
  getinv <- function() inv #returns the inverse, inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) #returns the 'special matrix' containing all of the functions just defined
} 


## The second function is used to calculate the inverse of the matrix
## created with the first function, but first checks to see if the inverse
## has already been calculated - and if so - gets the inverse and skips the
## computation. Otherwise, it calculates the inverse and sets the value
## in the setInverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}