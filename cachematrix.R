
## This function creates a matrix, sets and gets the value of the matrix, sets and gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
         x <<- y
         m <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) m <<- solve
     getsolve <- function() m
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
 }


## The following function calculates the inverse of the table created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets it from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the table and sets the value of the inverse in the cache via the setsolve function.

cachsolve <- function(x, ...) {
     m <- x$getsolve()
     if(!is.null(m)) {
         message("getting cached data")
         return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setsolve(m)
     m
        ## Return a matrix that is the inverse of 'x'
}
