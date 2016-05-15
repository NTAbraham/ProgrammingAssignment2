## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix())
  {
  inv <- NULL
  set <- function(y) #reset matrix
  {
    x <<- y  #copying value of y to x
    inv <<- NULL  #resetting value of inv to null
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}




##Write a short comment describing this function

##This function computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. 
##If the inverse has already been calculated 
##(and the matrix has not changed), then cacheSolve should retrieve
##the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 
  inv <- x$getInverse()
  
  if (!is.null(inv)) #checking to see if the matrix is the same
    {
    message("getting cached data")
    return(inv)   #returns the old result
    }
  
  mat <- x$get()   #retrieve the untoched matrix
  inv <- solve(mat, ...)  #invert the matrix
  x$setInverse(inv)  #assign inverted matrix
  inv   
}
