## The below functions cache the inverse of a matrix

## This function defines a matrix with the four functions that can set & get
## its value and set & get its inverse if the inverse has been calculated once.

makeCacheMatrix <- function(x = matrix()) {
  # Return a list that 
  # 1 set the value of the matrix
  # 2 get the value of the matrix
  # 3 set the value of the inverse
  # 4 get the value of the inverse
  
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  sets <- function(solve) s <<- solve
  gets <- function() s
  list(set = set, get = get,
       sets = sets,
       gets = gets)
}


## This function returns the inverse of a matrix if it has been cached before.
## If there is no cached value, this function calculates the inverse and cache it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' if there is calculated inverse
  s <- x$gets()
  if (!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  ## Calculate the inverse of a matrix 'x' if there is no calculated inverse
  data <- x$get()
  s <- solve(data, ...)
  # Cache it
  x$sets(s)
  # Return the inverse of 'x'
  s
}
