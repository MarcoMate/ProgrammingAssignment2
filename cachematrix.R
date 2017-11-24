makeMatrix <- function(x = matrix()) {
  # create a special "matrix", which is really a list containing a function to
  #set the value of the matrix
  #get the value of the matrix
  #set the value of the inverse of the matrix
  #get the value of the inverse of the matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheinverse <- function(x, ...) {
  # return a Matrix that is inverse of x
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

