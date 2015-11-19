# Main Function, with 'x' defined as a matrix. Example:
# a=makeCacheMatrix(matrix(c(2,4,5,3),2,2))
# IMPORTANT - You need to use a quadratic matrix - 2x2, 3x3, 4x4...
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # the 'set' sub-function will set a new matrix for a new computing task.
  # example of usage:
  # a$set(matrix(c(3,4,5,6,4,8,7,9,4),3,3))
  set <- function(y = matrix(...)) {
    
    x <<- y
    m <<- NULL
  }
  # the 'get' sub-function will print the computed matrix. 
  # Example:
  # a$get()
  get <- function(){
    x
  }
  # The 'setsolve' subfunction will compute the inverse of the matrix.
  # Example:
  # a$setsolve()
  setsolve <- function(){
    m <<- solve(x)
  } 
  # The 'getsolve' subfunction will print the inverse of the matrix calculated.
  # Example:
  # a$getsolve()
    getsolve <- function(){
    m <<- solve(x)
    m
  }
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#cachesolve function:
cacheSolve <- function(x, ...) {
  # this function will search for the computed inverse matrix.  If it could not 
  # find it(not a NULL result, because 'm' was already calculated...)
    m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    # the function will 'get' the cached result in 'x$getsolve()'
    return(m)
  }
   # I it is not calculated yet, the function will compute the inverse of the matrix!
  m <- solve(x$get())
  x$setsolve()
  m
  # Voilà!
}