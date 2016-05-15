##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix())
  {
  inv <- NULL  ## initializing
  set <- function(y) 
    {
    x <<- y #reassigning the value to a diff variable
    inv <<- NULL #initializing NULL to the variable
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already 
##been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) 
  {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv))  ##checking to see if it has the same value
    {
    message("getting cached data")
    return(inv) #return the inverted matrix
  }
  mat <- x$get() 
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}