## R programming assignment 2
## Peter Holberton

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() {
		x
	}
	setinverse <- function(i) {
		inverse <<- i
	}
	getinverse <- function() {
		inverse
	}

	# Return the wrapper object containing the getter/setter
	# pairs for the matrix and its inverse
	list(set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix().
## If the inverse has already been calculated (and the matrix has not changed),
## the inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
	inverse <- x$getinverse()
	if (!is.null(inverse)) {
		# Use the already cached value
		return(inverse)
	}
	m <- x$get()

	# calculate the inverse
	inverse <- solve(m, ...)

	# cache the newly calculated value
	x$setinverse(inverse)

	# return the newly calculated value
	inverse
}
