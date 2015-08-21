## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL

  ## initialize the lexically scoped variables, namely, x and i, 
  ## that contains the matrix and inverse respectively
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## get the matrix
  get <- function() x
  
  ## set the inverse results
  setinverse <- function(solve) i <<- solve
  
  ## get the inverse results
  getinverse <- function() i

  ## return the special matrix that has cached its inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the "special matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {

    ## access the cache (lexically scoped variable)
  i <- x$getinverse()
  
  ## if the matrix is found in the cache, post a message saying that the matrix is found in the cache
  if(!is.null(i)) {
    message("getting cached data")
    
    ## get the corresponding inverse of the matrix that is already computed and stored
    return(i)
  }
  
  # get the special matrix instance
  data <- x$get()
  
  ## compute  the inverse the matrix
  i <- solve(data, ...)

  ## Cache the results in the lexically scoped variable(i)
  x$setinverse(i)

  ## return the results stored in the local variable
  i  
}
