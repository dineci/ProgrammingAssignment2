## Put comments here that give an overall description of what your
## functions do

    ## pair of functions that cache the inverse of a matrix

## Write a short comment describing this function
    ## This function creates a special "matrix" object 
    ## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<-y
    inverse <<-NULL
  }
  get <-function() x
  setinverse <- function(solver) inverse <<- solver
  getinverse <- function () inverse
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## Write a short comment describing this function
    ## This function computes the inverse of the special
    ## "matrix" returned by makeCacheMatrix above. If the inverse has
    ## already been calculated (and the matrix has not changed), then
    ## the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  ## for this assignment, assume that the matrix supplied is always invertable
  ## however, for other situations, you may need to add code to make
  ## sure x is square, otherwise solve(x) will not work
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
