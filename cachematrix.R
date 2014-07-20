## Implementation matrix inverse which is using cache for the performance optimization

## Initializes and returns the object that keeps the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv) 
}


## gets the inverse matrix by using the cache

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)){
		return(inv)
	}
	data <- x$get()
	inv <- solve(x$get())
	x$setinv(inv)
	inv
}
