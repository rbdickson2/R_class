## Robert Dickson November 2015
## these functions are doing time consuming computations which cache the results
## so they can be looked up later instead of computing them again.
## These functions compute and cache the inverse of a matrix, which retrieves
## the inverse from the cache directly
## thank you. 

makeCacheMatrix <- function(x = matrix()) {
  ## @x: a square invertible matrix
  ## return: a list containing functions to
  ##              1. i first set the matrix
  ##              2. then i get the matrix
  ##              3. secondly i set the inverse
  ##              4. now we can get the inverse
  ##         my list is used as the input to cacheSolve()
  
  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # this will be different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## function returs: inverse of the original matrix input to makeCacheMatrix()
## @x: output of makeCacheMatrix()

cacheSolve <- function(x, ...) {
 
  ## November 2015 rbd 
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # here i sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}
