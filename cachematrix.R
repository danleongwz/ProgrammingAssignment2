## R Programming Assignment 2: Lexcical Scoping
## Create a function to store a matrix and also cache the inverse

## Creating a function that can cache a matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## Computing the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)){
        	message("processing")
        	return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
