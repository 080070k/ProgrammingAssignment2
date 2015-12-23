
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  ## Step 1: Initialize the inveser parameter
  inv <- NULL
  
  #Step 2: Create set function; assign y to x which is in another environment; similarily, assign NULL to inv
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## step 3: Create get function; return x
  get <- function() x
  
  ## Step 4: Create set inverse function; generate inverse using funciton "solve" and return the result
  setInverse <- function(inverse) inv <<- solve(x)
  
  ## Step 5: Create get inverse function; return inverse
  getInverse <- function() inv
  
  ## Step 6: Create a list containing the above funcitons
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  ## Step 1: Try to call getInverse() funciton and assign it to inv
  inv <- x$getInverse()
  
  ## Step 2: Check if inv is null; if it is not NULL, that means it has cached inverse -> just return inv; otherwise, we need to re-calculate the inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Step 3: retrieve the matrix data
  data <- x$get()
  
  ## Step 4: generate inverse
  inv <- solve(data, ...)
  
  ## Step 5: cache the inverse; this is important as we may save a lot of effort for all future calls
  x$setInverse(inv)
  
  ## Step 6: return inverse
  inv
}
