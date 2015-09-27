# Programming Assignment 2 by Dmytro Arkhypenko

## The function makes a matrix-like object 
## which is able of caching its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## The function returns inverse of input object previously 
## constructed by makeCacheMatrix. 
## If the inverse is requested first time - caculates and caches
## If it is not the 1st time the inverse is requested - cached value is returned

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}