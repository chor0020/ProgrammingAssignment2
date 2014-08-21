## Put comments here that give an overall description of what your
## functions do

## Creates a list of functions to be used to cache an inverse matrix
## set sets the non-inverted matrix which is chached
## get returns the non-inverted matrix
## setinverse caches the inverse matrix that was passed into the setinverse
## getinverse returns NULL if the inverse has not yet been calculated
makeCacheMatrix <- function(x = matrix()) {
	ix <- NULL
      set <- function(y) {
            x <<- y
            ix <<- NULL
      }
      get <- function() x
      setinverse <- function(inv) ix <<- inv
      getinverse <- function() ix
      list(set = set, get = get,
           setinverse = setinverse ,
           getinverse = getinverse )
}


## x is a list of functions created with makeCacheMatrix
## Extra arguments into this function will be passed into the solve function
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	ix <- x$getinverse()
      if(!is.null(ix)) {
            message("getting cached data")
            return(ix)
      }else {
      	data <- x$get()
      	ix <- solve(data, ...) ## inverse of data
      	x$setinverse(ix)
      	ix
	}
}
