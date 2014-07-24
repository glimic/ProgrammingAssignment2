## These functions are used to create special matrix objects that can cache its inverse and then,
## and if a inverted matrix has been already calculated, retrive it from the cache. 

## makeCacheMatrix creates a matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setin <- function(cacheSolve) s <<- cacheSolve
  getin <- function() s
  list(set = set, get = get, setin = setin, getin = getin)
}


## cacheSolve computes the inverse of matrix from precious function.
## If the inverse has already been calculated, then the the inverse is retrived from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getin()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data) %*% data
  x$setin(s)
  s}
