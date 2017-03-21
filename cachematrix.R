## Create a special 'matrix' object that can cache its inverse.
## Set value of matrix, Get value of matrix, 
## set value of inverse, get value of inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {  
    x<<-y
    inv<<-NULL
  }
 get<-function() x
 setinv <- function(inverse) inv <<- inverse
 getinv <- function() inv
 list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if(!isnull(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  
  return(inv)
  
  ## Return a matrix that is the inverse of 'x'
}
