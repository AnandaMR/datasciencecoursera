#Assignment: Caching the inverse of a matrix

#makeCacheMatrix: This function creates a special "matrix" object that 
#can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


#cacheSolve: This function computes the inverse of the special "matrix"
#returned by makeCacheMatrix above. If the inverse has already been 
#calculated (and the matrix has not changed), then the cachesolve should
#retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}

#Example
# > n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
# > n1
# [,1] [,2]
# [1,]    6    8
# [2,]    2    4
# > aMatrix <- makeCacheMatrix(n1)
# > aMatrix$get()               # retrieve the value of x
# [,1] [,2]
# [1,]    6    8
# [2,]    2    4
# > aMatrix$getsolve()           # retrieve the value of inv, which should be NULL
# NULL
# > cacheSolve(aMatrix)          # notice solve calculated is solve of n1
# [,1]  [,2]
# [1,]  0.50 -1.00
# [2,] -0.25  0.75
# > aMatrix$getsolve()           # retrieve it directly, now that it has been cached
# [,1]  [,2]
# [1,]  0.50 -1.00
# [2,] -0.25  0.75
#With new matrix
# > m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
# > aMatrix$set(m1)          # reset value with a new matrix
# > cacheSolve(aMatrix)          # notice solve calculated is solve of m1, not n1
# [,1] [,2]
# [1,]    6    8
# [2,]    2    4
# > aMatrix$getsolve()           # retrieve it directly, now that it has been cached
# [,1] [,2]
# [1,]    6    8
# [2,]    2    4
