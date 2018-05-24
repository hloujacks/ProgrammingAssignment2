## These functions allow the inverse of a matrix to be cached, 
## and then retrieved if needed again. Enter input in the form:
## cacheSolve(makeCacheMatrix(MyMatrix))

## Creates functions (get and set) that make it possible to cache an object
## (in this case it will be the inverse of a matrix)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                             ## Initiates i
  set <- function(y){
    x <<- y
    i <- NULL                     ## 'Resets' i - clearing anything from a previous execution
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## First checks if the inverse of matrix x is already stored in the cache, 
## if not, computes inverse of x (i) and returns it

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  return(i)
}