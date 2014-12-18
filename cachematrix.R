## These functions are developed to solve Coursera R Programming course second programming assignment
## They calculate the inverse of a square matrix, and return the result from the cache if it has already been calculated

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ### m will be the invert matrix and is reset to NULL every time makeCacheMatrix is called

  get <- function() {x} ## returns the value of the original matrix
  setinv <- function(solve) {m <<- solve} ## called by cacheSolve the first time the function is called
  getinv <- function() {m} ## called subsequent times when the function cacheSolve is called
  
  list(get = get, 
       setinv = setinv, 
       getinv = getinv)
}
  

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
