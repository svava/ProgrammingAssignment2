## These functions makeCacheMatrix and cacheSolve show examples of 
## lexical scoping in R. 



## This makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse. It also stores a copy of the original matrix for later 
## comparison in a global variable z.

makeCacheMatrix <- function(x = matrix()) {
## First, check to see if a matrix was passed
if (is.matrix(x)== TRUE) {
## OK, now we know we have a matrix and can get its inverse.

## Here we are storing a copy of x to the Global environment
  assign("z", x, envir = .GlobalEnv) 
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
  
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}
else {
  message("Argument needs to be a matrix. Try again.")
}
}


## This cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x["getinverse()"]
## Test to see if we already computed the inverse of 'x', and if 
## 'x' has not changed as compared to the z global variable stored 
## by makeCacheMatrix

    if((!is.null(m))& (x==z)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x[[setinverse(m)]]
  m
}
