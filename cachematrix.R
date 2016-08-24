## makeCacheMatrix and cacheSolve functions operate on a list which contain the functions to access the 
## cache values of the matrix X and its inverse invX to reduce computing time. 


## makeCacheMatrix returns a list with the functions needed to set and get 
## the internal values of x and its inverse invX. 
## When a new CacheMatrix is created its inverse is set to NULL 

makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  set <- function(y) {
    x <<- y
    invX <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invX <<- inverse
  getinverse <- function() invX
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of a cacheMatrix matrix.
## If the inverse matrix has been calculated before it returns the previously calculated value stored by 
## using the cacheMatrix function getinverse and sends a message.
## If the inverse matrix has not been calculated before calcultes the inverse matrix and stores its value using
## the cacheMatrix function setinverse.
#
## (...) allow to pass addtional parameters to function solve()

cacheSolve <- function(x, ...) {

  invX <- x$getinverse()
  if(!is.null(invX)) {
    message("getting cached data")
    return(invX)
  }
  data <- x$get()
  invX <- solve(data, ...)
  x$setinverse(invX)
  invX
}
