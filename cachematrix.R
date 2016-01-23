# 'makeCacheMatrix' is, in fact, a function which contains a list of other 
# functions (set, get, setinverse and getinverse):
# - 'set' allows us to assign specific matrix to x 
# - 'get' returns the value of x
# - 'setinverse' gives us the opportunity to assign value to the new variable 'inv'. 
# It doesn't really calculate the inverse - only assigns given value to the variable.
# - 'getinverse' simply returns the value of 'inv' (which was assigned in the 'setinverse')

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
	        x <<- y
	        inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
		setinverse = setinverse,
	        getinverse = getinverse)
}

# 'cacheSolve' returns us the inverted matrix of the one, given as parameter. 
# First, it checks whether the inverse was calculated before. If true, function 
# uses the cached value. If false, it calculates the inverse anf returns it. 

cacheSolve <- function(x, ...) {
  	## Return a matrix that is the inverse of 'x'
  	inv <- x$getinverse()
  	if(!is.null(inv)) {
    		message("getting cached data")
    		return(inv)
  	}
  	data <- x$get()
  	inv <- solve(data, ...)
  	x$setinverse(inv)
  	inv
}
