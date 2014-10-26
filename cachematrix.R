## These functions are used to calculate and cache the inverses of matrices. 

## The makeCacheMatrix function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix(1:4,2,2)) {
		i <- NULL
## set the value of the matrix
		set <- function(y){
			x <<- y
			i <<- NULL
		}
## get the value of the matrix
		get <- function() x
## set the value of the inverse 
		setinv <- function(inv) i <<- inv
## get the value of the inverse 
		getinv <- function() i
		list(set = set, get = get,
             	setinv = setinv,
             	getinv = getinv)
}



## The cacheSolve function computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed, 
## then the cacheSolve function retrieves the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
## Return a matrix that is the inverse of 'x'
## Check if inverse has already been cached. If so then return cached inverse value.
		i <- x$getinv()
		if(!is.null(i)) {
                message("getting cached data")
                return(i)
        	}	
## Calculate inverse of the matrix
		data <- x$get()
        	i <- solve(data, ...)
## Set cache to inverse value
        	x$setinv(i)
## Return the inverse value
        	i
}
