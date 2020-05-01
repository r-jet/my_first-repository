## Put comments here that give an overall description of what your
## functions do

## the makeCacheMatrix function makes a list thats contains a vector to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the given matrix
## 4.get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cachesolve function checks if the inverse if the matrix is already calculated.
## if yes, it gets the inverse from the cache(m), otherwise it uses the solve 
## function to calculate the inverse

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
##an example, mat is an invertible matrix
mat<-matrix(c(-3,1,5,0),2,2)
solve(mat)
