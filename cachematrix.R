## Functions that cache the inverse of a matrix, 'makeCacheMatrix' and 'CacheSolve'
## We assume that that matrix is always invertible

## 'makeCacheMatrix' creates a special matrix object,
## returns a list containing four functions to set and get a matrix,
## passed as argument, and its inverse
makeCacheMatrix <- function(x = matrix()) {

    inv.x <- NULL
    
    # 1. 'set' sets the value of the matrix x, argument of 'makeCacheMatrix'
    # and inv.x inverse of matrix x, initialized to NULL when x changes
    set <- function(y) {
      x <<- y
      inv.x <<- NULL
    }
    
    # 2. 'get' gets the value of the matrix x,
    # argument of 'makeCacheMatrix'
    get <- function() x
    
    # 3. 'setinv' sets the value of a matrix inv.x to the value inv, 
    # passed as an argument
    setinv <- function(inv) inv.x <<- inv
    
    # 4. 'getinv' gets the value of the matrix 'inv.x'
    getinv <- function() inv.x
    
    # 'makeCacheMatrix' returns a list of functions
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
  }
}


## 'cacheSolve' computes the inverse of the special "matrix"
## returned by 'makeCacheMatrix'. 
## If the inverse has already been calculated, 
## then the 'cachesolve' should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  
  # Check if the inverse has already been calculated 
  # Use 'getinv' to get the value of inv.x
  inv.x <- x$getinv()
  
  # if it is not NULL has already been calculated
  # so print the message that the inverse is in cache and not calculated 
  # and return this value 
    if(!is.null(inv.x)) {
    message("getting cached data")
    return(inv.x)
  }
  
  # if inv.x is NULL, get the matrix to be inverted by 'get' function 
  data <- x$get()
  # Calculte the inverse using 'solve' function of base package
  inv.x <- solve(data, ...)
  # Set the value of the inverse in the cache using 'setinv' function 
  x$setinv(inv.x)
  
  #'cacheSolve' returns a matrix that is the inverse of 'data'
  inv.x      
        
}

