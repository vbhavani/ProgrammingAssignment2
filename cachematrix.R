## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the "special matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## access the cache
  i <- x$getinverse()
  
  ## if the matrix is found in the cache, post a message saying that the matrix is found in the cache
  if(!is.null(i)) {
    message("getting cached data")
    ## get the corresponding inverse of the matrix stored
    return(i)
  }
  
  # create special matrix
  data <- x$get()
  
  ## cache the inverse the matrix
  i <- solve(data, ...)

  ## Return a matrix that is the inverse of 'x'
  x$setinverse(i)
  i  
}
