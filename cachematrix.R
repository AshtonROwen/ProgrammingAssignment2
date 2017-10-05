## This function can create objects of type "makeCacheMatrix" which return a list
## of 4 functions and 2 objects. The objects are an invertible matrix (x), and 
## the inverse of this matrixc (i). The 4 functions are used to get and set these
## values. Note that the inverse is not actually calculated in this function. 
## (x) is an argument of the function and by default is set as an empty matrix. 
## When this function is used to create an object of type "makeCacheMatrix", the 
## values of (i) and (x) are stored in RAM or "cached" i.e. not thrown away, 
## so that if any of the 4 functions are called from the object, they can be 
## properly evaluated. This occurs because of R's lexical scoping rules - the 
## functions look for the values of the objects (i) and (x) in the environment in 
## which they were defined, and so that "environment" is preserved or cached so 
## that this can occur.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function takes as an argument an object of type "makeCacheMatrix".
## It calls the getinverse function from that object, which in return pulls
## the cached value of (i) to assess whether an inverse of the matrix (x)
## has been calculated. If yes, it returns the value, if not, it gets the matrix
## and calculates the inverse. It then caches the inverse, and returns it. 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i    
}