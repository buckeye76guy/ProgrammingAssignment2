## This functions takes a matrix and provide it with specific coordinates
## Given the matrix we create a list that contains the right parameters that can
## help us retrieve the matrix and its inverse without direct computation

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}
### --------------------------------------------------------------------------------- ###

## This is a version of the solve function that has access to different environments
## When The inverse of a matrix is being solved for the first time, this function
## has runtime similar to that of solve(). but when the same matrix is being
## manipulated, this function is much quicker and more efficient than the solve()
## function especially for large sized matrices.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # If m is null, meaning if its the first time we want to compute the inverse
  # of a specific matrix, we do the following
  
  data <- x$get() #This only go back to our global environment to pick x as we
                    #initiated it. Then we can calculate its inverse
  m <- solve(data, ...) # Here we get the inverse of the matrix
  x$setsolve(m) # This simply puts the result from the line above into one of the
                # coordinates of x. That way when we look for the value of the 
                # inverse for x we can quickly retrieve it
  m
}
