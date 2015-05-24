## The following functions work together to compute and cache the
## inverse of a matrix.

## makeCacheMatrix creates a special list that holds functions that
## can be used to set and get the value for a matrix and set and get
## the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y) {                                      ## Sets the value of the Matrix
    x <<- y
    i <<- NULL
  }
  
  get <- function() { x }                                   ## Stores the matrix in question
  setinverse <- function(inverse) { i <<- inverse }         ## Sets the inverse
  getinverse <- function() { i }                            ## Get the valur of the inverse
  
  return(list(set = set, get = get, setinverse = setinverse, ## Returns a list of functions
              getinverse = getinverse))                             ## that can be called.
}

## This function first checks to see if the inverse has already been
## calculated. If so, it skips the computation and returns what it 
## finds in memory with a message indicating that it is cached. If 
## not, it computes the inverse and stores it in memory.

cacheSolve <- function(x, ...){
  
  i <- x$getinverse()
  
  if(!is.null(i)) {                         ## Checks if inverse was cached
    message("getting chached data")   ## Notifies user that the inverse was cached
    return(i)                         ## Returns from memory
  }
  
  data <- x$get()                           ## If not, stores the matrix in data
  i <- solve(data, ...)                     ## Computes the inverse
  x$setinverse(i)                           ## Calls a function from x to cache the inverse
  return(i)                                 ## returns the inverse
  
}