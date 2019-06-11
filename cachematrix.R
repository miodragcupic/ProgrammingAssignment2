## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## first function makeCacheMatrix creates a special matrix which is in the end a list containing  a function to
## a) set the value of the matrix
## b) get the value of thr matrix
## c) set the value of the inverse matrix
## d) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
## second function cacheSolve checks first if we have 
## a calculated cached inversed matrix by checking if the variable "i" is empty, 
## it returns that one. if not it calculates an inversed matrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("Getting Cached Data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
