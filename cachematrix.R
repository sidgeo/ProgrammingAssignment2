## By Shiju George March 27 Rev 0

##The makeCacheMatrix is a function which takes a matrix as its primary 
## argument. A set of 4 related functions form the body of the function which
## serve to store the matrix inverse, retrieve the matrix inverse, retrieve the
## matrix and set a new matrix.The matrix inverse is calculated in the cacheSolve
## function

makeCacheMatrix <- function(x = matrix()) {
      
      m <- NULL
      
      set <- function(y) {
          x <<- y
          m <<- NULL
      }
      
      get <- function() x
      
      setinverse <- function(inverse) m <<- inverse
      
      getinverse <- function() m
    
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
      
}

## makeCacheMatrix must be run before running the cacheSolve function.
## The cacheSolve function checks to see if there is already a matrix inverse in 
## cache. If so, that matrix inverse is returned.  Otherwise, the matrix inverse
## is calculated for the main function argument of makeCacheMatrix, then stored in
## cache, and returned.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
  
      m <- x$getinverse()
       if(!is.null(m)) {
         message("getting cached data")
         return(m)
       }
      
      data <- x$get()
      m <- solve(data)
      x$setinverse(m)
      m
  
}
