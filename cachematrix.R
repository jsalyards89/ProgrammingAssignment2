
makeCacheMatrix <- function(x = matrix(nrow = 2,ncol = 2)) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvrs <- function(inverse) m <<- inverse
  getinvrs <- function() m
  
  list(set = set, get = get,
       setinvrs = setinvrs,
       getinvrs = getinvrs)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvrs()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvrs(m)
  m
}

