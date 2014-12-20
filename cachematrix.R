## You see two functions below. They are, together, used to compute  the inverse
## of a matrix if it  is not computed before. If the inverse of a matrix is already
## computed ancd cached, the second function returns the cached value, if not, the
## second function calculates the inverse, caches and returns the value.

## The function below takes a matrix and returns a list containing the values of
## functions, to get and set the value of the matrix and a parameter called inv.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  getmatrix <- function() { x }
  
  setinverse <- function(inverse) {inv <<- inverse }
  
  getinverse<- function() {inv}
  
  list( getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}


## This function takes a soecial matrix created by the above function and calculates its
## inverse if it is not computed before. In addition to computation, the function
## caches the inverse for future use. If the inverse has already been calculated, the message
## getting cache data is returned and the previously computed value is returned.
## This is done by the superposition <<- and setinverse function in above function
## makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting the cached data")
    return(inv)
  }
  data <- x$getmatrix()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
