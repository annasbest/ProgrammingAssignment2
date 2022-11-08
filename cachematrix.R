
## makeCacheMatrix: A function that creates a special "matrix" object that can
## cache its inverse. makeCacheMatrix first initializes a function argument x,
## as well as the argument dummy inside the function itself. Afterwards, I define
## the set and get functions for both variables x and dummy. At the end, each
## function returned to parent environment via a list.

makeCacheMatrix <- function(x = matrix()) {
  # cached data will be erased
  dummy <- NULL
  
  # set value of matrix
  set <- function(y) {
    # parent environment
    x <<- y 
    dummy <<- NULL}
  # set value of inverse matrix
  setinverse <- function(inverse) dummy <<- inverse
  
  # get value of matrix
  get <- function() x # for lexical scoping
  # get value of inverse matrix
  getinverse <- function() dummy
  
  # named elements to use $ operator
  list(set = set, 
       setinverse = setinverse
       get = get, 
       getinverse = getinverse
  )
}


# cacheSolve: Function that computes the inverse of the special "matrix" returned
## by makeCacheMatrix from above. If the inverse has already been calculated while
# the matrix did NOT change, then the cachesolve will retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  dummy <- x$getinverse()
  # returns to parent env if cached matrix is valid
  if(!is.null(dummy)) {
    message("getting cached matrix")
    return(dummy)
  }
  # executes if false
  data <- x$get()
  dummy <- inverse(data, ....)
  x$setinverse(data)
  dummy
}
