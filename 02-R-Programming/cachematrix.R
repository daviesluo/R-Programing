## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## this function works as getter and setter for the matrix
makeCacheMatrix <- function(x = matrix()) {
  #initialize an environment
  m <- NULL
  
  #initialize x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x #return x as is
  setInverse <- function(inverse) m <<- inverse #assign the inversed matrix
  getInverse <- function() m # get the reversed matrix
  
  #return a list of functions
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## Write a short comment describing this function

#this function takes a matrix and return its inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #get the inverse from the list returned when caching the matrix
  m <- x$getInverse() 
  
  #check if the matrix is null in case if it wasn't initialized 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #get the data  from x nd solve it
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

