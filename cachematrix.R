# the first function creates a matrix which gets and sets the values of it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
set <- function(y) {
  x <<- y
  inv <<- NULL
}

get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set,
     get = get,
     setInverse = setInverse,
     getInverse = getInverse)

}


## The cachesolve function takes the matrix creates from function 1 and computes the cache of the inverted matrix.Because makecachematrix already calculates the inverse values, it directly retrieves the inverse of the cache 
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
if (!is.null(inv)) {
  message("getting cached data")
  return(inv)
}
mat <- x$get()
inv <- solve(mat, ...)
x$setInverse(inv)
inv
}

