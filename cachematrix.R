
##These functions let you cache the inverse of a matrix in the case where you don't want to continually re-calculate the inverse.


## Use the makeCacheMatrix function to compute the inverse of a matrix and then cache it.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Use the casheSolve function to calculate the inverse of the matrix that you get as a result of running the makeCacheMatrix function.
## If the inverse of the matrix is already cached, it will return the cached value. If it is not cached, it will calculate the inverse.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
  
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
