## The function makeCacheMatrix creates a list containing functions to
## 1. set the value of a matrix
## 2. get the value of the stored matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      sol <- NULL
      
      ## nested function to set the value of a matrix
      set <- function(y) {
            x <<- y
            sol <<- NULL
      }
      
      ## nested function to get the value of the stored matrix
      get <- function() x
      
      ## nested function to set the value of the inverse of the matrix
      setsol <- function(solution) sol <<- solution
      
      ## nested function to get the value of the inverse of the matrix
      getsol <- function() sol
      
      ## returning the above functions as a single list
      list(set = set, get = get,
           setsol = setsol,
           getsol = getsol)
}


## The function cacheSolve calculates the inverse of the matrix inserted into  
## the function makeCacheMatrix (with set()). However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates the inverse 
## of the data and sets the inverse matrix in the cache via the setsol function.

cacheSolve <- function(x, ...) {
      sol <- x$getsol()
      
      ## if the value returned by getsol() is not null, i.e. the result is stored 
      ## in the cache, it is returned from the function
      if(!is.null(sol)) {
            message("getting cached data")
            return(sol)
      }
      
      ## if the cache does not have the result, it is calculated and stored 
      ## in the cache, and then returned 
      data <- x$get()
      sol <- solve(data, ...)
      x$setsol(sol)
      sol
}
