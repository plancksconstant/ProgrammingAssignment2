## These functions are for calculating the inverse 
## of a matrix, if the inverse does not already exist
## in the cache.  

## This function returns a list of functions.  These
## will set a matrix, get the matrix, set the matrix
## inverse, and get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
		im <- NULL
		set <- function(y) {
			x <<- y
			im <<- NULL
		}
		get <- function() x
		setinvm <- function(invmat) im <<- invmat
		getinvm <- function() im
		list(set = set, get = get,
			setinvm = setinvm,
			getinvm = getinvm)
}


## This function will check to see if the matrix
## inverse is stored in the cache and return it, 
## if it is there, or calculate inverse, if it 
## is not in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          im <- x$getinvm()
          if(!is.null(im)) {
          	message("getting cached data")
          	return(im)
          }
          data <- x$get()
          im <- solve(data, ...)
          x$setinvm(im)
          im
}
