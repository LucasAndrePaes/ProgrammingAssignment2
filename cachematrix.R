
## The function makeCacheMatrix create one matrix e a set of functions to operate on this.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() x
  setInverseMatrix <- function(solve) m <<- solve
  getInverseMatrix <- function() m
  list(set = set, getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## The function cacheSolve verify if the matrix inverse create in makeCacheMatrix is in cache. If yes, return its without calculate the inverse. If no, calculate its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getMatrix()
  m <- solve(data, ...)
  x$setInverseMatrix(m)
  m
}
