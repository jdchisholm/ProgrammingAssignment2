## The makeCacheMatrix takes in a matrix object but also consists of a number of other functions that can be used to set and get an entered matrix or inverse matrix. None of the functions calculates the inverse, an entered inverse matrix is just held in the "cache"
## set function - sets the matrix, auto set when a matrix is passed into the function, but x$set can be used to set new matrix any time.
## get function - calling this function (x$get()) will return the matrix currently set in the x$set function
## setinv - function allows user to set/enter an inverse matrix to cache
## getinv - calling this function will return the inverse matrix entered in x$setinv

## Function takes in a matrix, or allows users to set a new matrix at anytime. The function however does not calculate the inverse of the matrix, it just holds an inverse matrix that the user enters via x$setinv. If no value is entered into x$setinv, then the inverse variable is left as NULL

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## This function calls the getinv function from makeCacheMatrix and checks the value. If x$getinv() is not NULL (i.e., an inverse matrix was entered in makeCacheMatrix), then the function returns the cached inverse matrix. If no matrix was entered into x$setinv (i.e., x$getinv returns NULL), then the function determines and returns the inverse of whatever matrix is returned by x$get() (i.e., the matrix entered in the makeCacheMatrix function or set using x$set)   

cacheSolve <- function(x, ...) {
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
