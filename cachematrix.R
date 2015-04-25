#makeCacheMatrix is a function that returns a list of functions
#Its purpose is to store a matric and cached value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
     
     #Holds cached value
     cache <- NULL
     
     #store a matrix
     setMatrix <- function(val) {
          x <<- val
          cache <<- NULL
     }
     
     #returns stored matrix
     getMatrix <- function() {
          x
     }
     
     #cache argument
     cacheInverse <- function(arg) {
          cache <<- arg
     }

     #get the cached value
     getInverse <- function() {
          cache
     }
     
     #return a list. Each named element of the list is a function
     list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


#The following function calculates the inverse of a special matrix created with makeCacheMatrix

cacheSolve <- function(y, ...) {
     #get the cached value
     inverse <- y$getInverse()
     #if a cached value exists return it
     if(!is.null(inverse)){
          message("getting cached data")
          return(inverse)
     }
     
     #otherwise get the matrix, calculate the inverse and store it in the cache
     data <- y$getMatrix()
     inverse <- arg(data)
     y$cacheInverse(inverse)
     
     #return the inverse
     inverse
}
