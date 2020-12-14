# Programming-Assignment2
## The function below will be used to check the inverse of the matrix
## If not found, compute the inverse using solve()

## This function uses lexical scoping
## It returns a list of four functions set, get, setinverse and getinverse

makecacheMatrix <- function(x = matrix()){
  
  i <- NULL
  set <- function(y) {
    
    x <<- y
    i <<- NULL 
  }
  get <- function() x
  setinverse <- function(solve) i <- solve
  getinverse <- function() i
  list(set = set,
       get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function calculates the inverse of the matrix but first checks the cache

cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  if (!is.null(i)) {
    
    message ("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}