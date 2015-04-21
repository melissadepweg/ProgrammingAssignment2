## makeCacheMatrix creates a matrix to invert
## cacheSolve inverts the called matrix

## makeCacheMatrix function, creates a special matrix which sets the value of the matrix, gets the value of the matrix,
## performs the solve function, and gets the results of the solve function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
  x <<- y
  m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve function calculates the inverse of the matrix using the above function, but first looks for a cached result
## If the inverse has already been calculated, it returns the existing inverse. Otherwise, it calculates it calling the function
## above and sets the value of the solve function in the cache to be used in the future.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data for matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
