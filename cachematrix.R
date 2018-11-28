## Function that returns a list of functions that can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) i<<-inverse
  
  getinverse <- function() i
  
  list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)

}


## Check if the inverse of the matrix has already been calculated. If it's already calculated,
## return the cached inverse, else calculatre the inverse and return.

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data)
  
  x$setinverse(m)
  
  m
}
