## Put comments here that give an overall description of what your
## functions do

## this function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvers <- function(invers) m <<- invers
  getinvers <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)

}


#This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already 
#been calculated, then the cachesolve 
#should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getinvers()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvers(m)
  m
  
}
