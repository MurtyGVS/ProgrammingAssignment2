## Cache the inverse of matrix first
## and then every time the function is used, verify whether 
##cache already has the inverse

## Cache the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL ## Sets the value of NUll to inv
  
set <- function(y) {
	x <<- y ## Caches the matrix input so that whe the function is called in cachesolve
          ## it can be verified whether it has any value or nor
	inv <<- NULL ## Nullifies m so that if it has any matrix, it is reset
}

get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse<- function() inv
list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## The following function returns the inverse of the matrix.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

