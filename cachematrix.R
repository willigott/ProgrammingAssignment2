## The functions 'makeCacheMatrix' and 'cacheSolve' can be used to cache the
## potentially costly matrix inversion.

## A test of the function can be found below the functions.

## 'makeCacheMatrix' creates a matrix object which can cache the matrix' inverse.
## It returns a list of functions that allow to set and get the matrix value,
## and to set and get the inverse of this matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #inv will store inverse matrix
  # set value of our matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get the value of our matrix
  get <- function() x
  # set the value of the matrix' inverse
  setinv <- function(invMat) inv <<- invMat
  # set the value of the matrix' inverse
  getinv <- function() inv
  # return a list of functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## 'cacheSolve' computes the inverse of a matrix created by 'makeCacheMatrix'. If its inverse has 
## already been calculated before, it returns the cached inverse along with a message.

cacheSolve <- function(x, ...) {
  # get the cached inverse of x
  inv <- x$getinv()
  # if this cached inverse is not NULL return it along with a message
  if(!is.null(inv)) {
    message("getting cached inverse matrix...")
    return(inv)
  }
  # If this cached inverse is NULL then we get the matrix value, calculate the matrix'
  # inverse, cache it and return it:
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}

# some testing using an example from http://www.mathwords.com/i/inverse_of_a_matrix.htm:

# myMatrix <- makeCacheMatrix() #create matrix object
# myMatrix$set(matrix(c(4,3,3,2),2,2)) # a 2x2 matrix
# myMatrix$get() #get the value of the matrix
# cacheSolve(myMatrix) #get inverse
# cacheSolve(myMatrix) # get again the inverse; this time, a message will be displayed

# This will result in the following output:

# > myMatrix <- makeCacheMatrix()
# > myMatrix$set(matrix(c(4,3,3,2),2,2)) # a 2x2 matrix
# > myMatrix$get()
# [,1] [,2]
# [1,]    4    3
# [2,]    3    2
# > cacheSolve(myMatrix)
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
# > cacheSolve(myMatrix)
# getting cached inverse matrix...
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4

# As one can see, the inverse is correct and the message is only displayed for the second call
# of 'cacheSolve'.