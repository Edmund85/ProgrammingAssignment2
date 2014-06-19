## The makeCacheMatrix function creates a special "matrix", which is  
## really a list containing a function to;
## 1.set the value of the matrix (using set function)
## 2.get the value of the vector (using get function)
## 3.set the value of the mean (using setinverse function)
## 4.get the value of the mean (using getinverse function)

## The cacheSolve function calculates the mean of the 
## special "vector" created with the above function. However, it first 
## checks to see if the mean has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the 
## mean in the cache via the setmean function.

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	x <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <-function() x
	setinverse <- function(solve) x <<- solve
	getinverse <- function() m
	list (set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if (!is.null(m)) {
		message("getting cached data")
		return (m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m

}
