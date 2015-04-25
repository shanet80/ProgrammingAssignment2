#makeCacheMatrix is a function that returns a list of functions
#Its purpose is to store a matric and cached value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
     
     #Holds cached value
     cache <- NULL
     
     #store a matrix
     set <- function(y) {
          x <<- y
          cache <<- NULL
     }
     
     #returns stored matrix
     get <- function() {
          x
     }
     
     #cache argument
     setInverse <- function(arg) {
          cache <<- arg
     }

     #get the cached value
     getInverse <- function() {
          cache
     }
     
     #return a list. Each named element of the list is a function
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


#The following function calculates the inverse of a special matrix created with makeCacheMatrix

cacheSolve <- function(x, ...) {
     #get the cached value
     inverse <- x$getInverse()
     #if a cached value exists return it
     if(!is.null(inverse)){
          message("getting cached data")
          return(inverse)
     }
     
     #otherwise get the matrix, calculate the inverse and store it in the cache
     data <- x$get()
     inverse <- solve(data, ...)
     x$setInverse(inverse)
     
     #return the inverse
     inverse
}
