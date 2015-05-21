## Functions allow a user to create a special object that can cache
## the inverse of a matrix and then utlizie that cache if exists.

## This function creates a special function list based on a
## specific matrix and allows the user to store a cache of
## said inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setImatrix <- function(Imatrix) m <<- Imatrix
  getImatrix <- function() m
  list(set = set, get = get,
       setImatrix = setImatrix,
       getImatrix = getImatrix)
}


## This function takes the object created from the makeCacheMatrix
## and allows the user to check for a cache of the inverse matrix.
## If one exists, the cache
## is printed, else an inverse matrix is created.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getImatrix()
  if(!is.null(m)) {
    print("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setImatrix(m)
  m
}
