## The purpose of these functions is to save computation time by "reusing" matrix inverses by 
## first calculating them and then caching them for later use.
## The first function creates a cache for a matrix while the second function will compute the inverse if one
## is not already cached, or simply access and return the cached version if it has already been calculated.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  #inverse initialise
  inv <- NULL
  
  #f1
  #set matrix x as y, reset inverse 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #f2
  #return matrix x
  get <- function() x
  
  #f3
  #set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  #f4
  #return inverse
  getinverse <- function() inv
  
  #list of the four functions is returned
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), the cached inverse is retrieved.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  #if there already is an inverse, skip over the computation part,
  #uses the cached inv
  if(!is.null(inv)) {
    message("getting cached data")
    inv
  }
  
  #otherwise, inverse of x is calculated in the traditional, expensive way
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
