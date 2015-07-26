## The following functions attempt to reduce the cost of matrix inversion 
## computations by caching the inverse of the matrix rather than calculating it 
## repeatedly

## makeCacheMatrix creates a special "matrix" object that can cache the matrix's
## inverse. This special "matrix" is actually a list containing functions to
## (a) set the value of the matrix, (b) get the value of the matrix, (c) set the 
## value of the inverse matrix, and (d) get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <-  function(y){
    x <<- y
    inv <<- NULL
      ##sets the matrix, prepares it for the following functions
  }
  get <- function() x
    ##returns the matrix
  setInverse <- function(inverse) inv <<- inverse
    ##sets the inverse of the matrix
  getInverse <- function() inv
    ##returns the inverted matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" that results from 
## makeCacheMatrix. This function first determines whether or not the inverse
## has already been computed and cached.If already cached, the inverse is retrieved.
## If not, the function computes and  caches the inverse matrix.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse() 
    ## computing the inverse of x, which is the special "matrix" object from 
    ## makeCacheMatrix
  if(!is.null(inverse)) {   
    message("retrieving cached inversion")
    return(inverse)
      ## if the inverted matrix is already cached, this returns that inverse
  }
  object <- x$get()
  inverse <- solve(object)
  x$setInverse(inverse)
  inverse
    ## if the inversion is not cached, this computes the inversion, caches it,
    ## and returns the inverted matrix
        ## Return a matrix that is the inverse of 'x'
}
