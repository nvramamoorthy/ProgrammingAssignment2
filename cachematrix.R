
## These  two functions  given below , are used to cache the inverse of a matrix. 

##  makeCacheMatrix creates a list containing a function to
## First set the value of the matrix
## Then get the value of the matrix
##  Then  set the value of inverse of the matrix
##  Finally get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Following function  returns inverse of the matrix. 
## It checkes if the inverse is done already.
## If it is not done , it computes the inverse, and set the values in cache by excuting set inverse function
## The function assumes the matrix is always invertible

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if (!is.null(inv)) {
		message("getting cached data.")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv

}




