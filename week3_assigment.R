## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix(sample(1:30,2),3,3)) {
  set.null <- NULL
  set <- function(y) {
    x <<- y
    set.null <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) set.null <<- solve
  getsolve <- function() set.null
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  set.null <- x$getsolve()
  if(!is.null(set.null)) {
    message("getting inversed matrix")
    return(set.nul)
  }
  data <- x$get()
  set.null <- solve(data, ...)
  x$setsolve(set.null)
  set.null
}
