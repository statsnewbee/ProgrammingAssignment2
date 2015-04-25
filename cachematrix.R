## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache its input and inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # sets the value of m to NULL (provides a default if cacheSolve has not yet been used)
  y <- NULL # sets the value of y to NULL (provides a default if cacheSolve has not yet been used)
  setmatrix <- function(y) { #set the value of the matrix
    x <<- y ## caches the input matrix so that cacheSolve can check whether it has changed (note this is within the setmatrix function)
  
  }
  getmatrix <- function() 
    x #get the value of the matrix
  setinverse <- function(solve) 
    m <<- solve # compute the value of the inverse 
  getinverse <- function() 
    m # get the value of the inverse and caches it within the setinverse function
  list(setmatrix = setmatrix, getmatrix = getmatrix, # creates a list to house the four functions
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 # Need to compare matrix to what was there before!
  m <- x$getinverse() # if an inverse has already been calculated this gets it
   
  if(!is.null(m)){ # check to see if cacheSolve has been run before
    if(identical(x$setmatrix(m),x$getmatrix())) { # check that matrix hasn't changed, and if it hasn't, sends a text message and returns the cached matrix
     message("getting cached data")
      m <- x$getinverse() 
    }
    return(m)
  }
  # otherwise - this part of the code computes the inverse and caches it and returns it
  y <- x$getmatrix() # run the getmatrix function to get the value of the input matrix
  x$setmatrix(y) # run the setmatrix function on the input matrix to cache it
  m <- solve(y, ...) # compute the value of the inverse of the input matrix
  x$setinverse(m) # run the setinverse function on the inverse to cache the inverse
  m # return the inverse		
		
		
}
