## This function creates a matrix obect that caches its inverse.
## 

## makeCacheMatrix returns a list of functions that set the matrix,
## get the matrix, set the inverse, and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
      matrinv <- NULL
      set <- function(y) {
            x <<- y
            matrinv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) matrinv <<- inverse
      getinverse <- function() matrinv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## returns the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrinv <- x$getinverse()
        if(!is.null(matrinv)) {
                message("getting cached data")
                return(matrinv)
        }
        matrix.data <- x$get()
        matrinv <- solve(matrix.data, ...)
        x$setinverse(matrinv)
        matrinv
}
