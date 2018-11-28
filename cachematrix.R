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


## Write a short comment describing this function

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
