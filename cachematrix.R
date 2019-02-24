## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 #set inverse to null 
   inv <- NULL
  set<- function(y) 
    {
    #assign x to y
    #assign null to inverse "inv"
    x <<- y
    inv <<- NULL
  }
  get<- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<- x$getinv()
  if(!is.null(inv)) {
    message("getting cache data")
    return(inv)
  }
  mat <- x$get()
  inv<- solve(mat, ...)
  x$setinv(inv)
  inv
}
