## This function creates a matrix
## It takes three arguments
## Values of the matrix elements, number of rows, number of columns

makeCacheMatrix <- function(x = numeric(), nrow = integer(), ncol = integer()) {
  s <- NULL
  set <- function(y, nrow, ncol) {
    x <<- matrix(y, nrow, ncol )
    s <<- NULL
  }
  get <- function() matrix(x,nrow, ncol)
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function returns the inverse of a matrix
## If the inverse of the matrix is calculated for the first time
## the compuations is made and it is cached
## When the inverse of the matrix is already calculated, the cached matrix
## is returned.


cachesolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
