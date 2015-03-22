## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #This function has 2 internal variables and 4 functions
  #variable x: Used for storing input matrix
  #variable inv: Used for storing inverse matrix
  #function set(): Used for initializing internal variables
  #function get(): Used for getting input matrix
  #function setinv(): Used for creating the inverse matrix
  #function getinv(): Used for getting the inverse matrix
  #list: Contains the list of functions
  inv <- NULL
  set <- function(y)
  {
    # sets x as input matrix, inverse matrix is null
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  #assigning the internal inverse variable to the variable inv
  inv <- x$getinv()
  
  #check if inv is calculated before by comparing it with NULL
  #if calculated before return without inverswing the matrix
  if(!is.null(inv)){
    message("Getting cached data")
    
    #already calculated, returning the inverse matrix
    return(inv)
  }
  
  #if inverse is not calcualted before calculate it
  message("Calculating inverse matrix")
  
  #assigning the internal matrix to data
  data <- x$get()
  
  #inversind data and assigning it to variable inv
  inv <- solve(data)
  
  #setting internal inverse matrix
  x$setinv(inv)
  
  #returning inverse matrix
  inv
}
