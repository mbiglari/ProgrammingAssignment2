## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# x is an invertible matrix
# Returns a list containing list of functions: (a) set a matrix, (b) get the matrix
# (c) set the invers of the matrix, (d) get the invese of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## Write a short comment describing this function
# x is a list of functions returned by makeCacheMatrix
# Return inverse of matrix from cache if already cached
# otherwise calculate the inverse and chache the inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

