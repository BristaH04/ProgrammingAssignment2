## The below two functions utilize the Lexical Scoping property of R
## in order to cache results of a function, for repeated use in other
## functions, without having to recommpute the values.  
## Relying on the cached values drastly reduces the processing power and time
## required to run functions for large data sets.

## The makeCacheMatrix takes a matrix (x) and creates a list of functions based on that matrix.
## This list of functions then becomes the input for the next function (cacheSolve); The functions
## in the list do the following:
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function returns the inverse of matrix (x). If this function 
## has not previously been run, it will calculate the inverse of matrix (x); 
## otherwise, it will rely on the cache value of the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
      data <- x$get()
      inv <- solve(data, ... )
      x$setinv(inv)
      return(inv)
}
