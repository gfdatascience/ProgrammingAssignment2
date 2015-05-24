## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly. The pair of functions below cache the inverse of a
## matrix.

# The makeCacheMatrix function creates a special "matrix",
# which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(
    set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

# This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has 
# already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
            ## Return a matrix that is the inverse of 'x'
i <- x$getinverse()
if(!is.null(i)) {
message("getting cached data")
return(i)
}
data <- x$get()
i <- solve(data, ...)
x$setinverse(i)
i  
  
}
