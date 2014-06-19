##These functions allow you to cache the inverse of a matrix.
##makeCacheMatrix gets the matrix and then sets it.
##It then creates the inverse of the matrix and sets the inverse.

## To call the functions, do the following(seelecture notes on functions:
##1. Get the matrix that you want to invert.  x<- matrix(c(2,9,4,23),ncol=2,nrow=2)
##2. Store the function:  z<-makeCacheMatrix()
##3. Pass the matrix to the function:  z$set(x)
##4. Run cacheSolve(z)     

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setmatrix <- function(solve) m <<- solve
     getmatrix <- function() m
     list(set = set, get = get,
          setmatrix = setmatrix,
          getmatrix = getmatrix) 

}

#This checks to see if the the inverse of the matrix has been
#calculated.  If it has, it returns the cached inverse.  If not, 
#inverts the matrix.
##

cacheSolve <- function(x, ...) {
     m <- x$getmatrix()
     if(!is.null(m)) {
          message("Getting the cached inverse.")
          return(m)
     }
     mymatrix <- x$get()
     m <- solve(mymatrix, ...)
     x$setmatrix(m)
     m
       
}
