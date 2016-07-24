
## Our assignment was to to write a pair of functions that cache the inverse of a matrix.
# I followed our given examples expliccitly making the neccassry changes t

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(minv) m <<- minv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


##  This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the inverse 
#from the cache.
#Computing the inverse of a square matrix can be done with the solve function in R. 
#We will be using this function explicityly within our chacheSolve function under the
#assumption that our matrix is a square invertible matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  dat <- x$get()
  m <- solve(dat, ...)
  x$setinv(m)
  m
}
#creating a 3x3 to see if the code works.
#test<-makeCacheMatrix(matrix(data=c(1,-2,3,-4), nrow=3, ncol=3))
#cacheSolve(test)


