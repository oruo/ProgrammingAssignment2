## Below are two functions that are used to create a special object
## that stores a matrix and caches its inverse

## Function that makes and gets cached matrix and its inverse. 
## It creates a special vector to set and get the values 
## of a matrix and of its inverse

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) iv <<- solve
  getinverse <- function() iv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function that calculates the inverse of the special "matrix" 
## created with makeCacheMatrix above. It first checks to see if the 
## inverse of the matrix has already been calculated. If so, it gets 
## the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the matrix and sets the value 
## of the inverse  matrix in the cache via the setinverse function

cacheSolve <- function(x, ...) {
  iv <- x$getinverse()
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  iv <- solve(data, ...)
  x$setinverse(iv)
  
  ## Return a matrix that is the inverse of 'x'
  iv
        
}
