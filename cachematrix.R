## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes a matrix as an argument and creates a set and get functions
## to access the original matrix provided in the arguments
## and also provides a setInverse and getInverse functions to set and get the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inv_par) inv <<- inv_par
	getInverse <- function() inv
	list(set=set, get=get,
   		setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
## this function takes the result of a makeCacheMatrix to first check if
## there is an alreay provided data about the inversed matrix and return it
## or otherwise calculate the inverse with the solve function store it and then returned it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if(!is.null(inv)){
		message("Getting data from the cache")
		return(inv)
	}
	matrx <- x$get()
	inv <- solve(matrx, ...)
	x$setInverse(inv)
	inv
}