##################################################################
##
## Programming Assignment 2: Caching the Inverse of a Matrix
##
##################################################################

makeCacheMatrix <- function(M = matrix()){

	## This function creates a 'special matrix', wich is a list 
	## containing the following items:
	## 
	## -        set : a function to set the value of the matrix
	## -        get : a function to get the value of the matrix
	## - setInverse : a function to set the value of the inverse matrix  
	## - getInverse : a function to get the value of the inverse matrix

	## Local variable to store the inverse matrix
	invM <- NULL

	## Function that sets 'M' to be the feeded matrix
	set <- function(matrix){
		M <<- matrix
		invM <<- NULL
	}

	## Function that returns the matrix
	get <- function(){
		M
	}

	## Function that sets 'invM' to be the inverse matrix feeded
	setInverse <- function(inverse){
		invM <<- inverse
	}

	## Function that returns the inverse matrix
	getInverse <- function(){
		invM
	}

	## Return a list as discribed at the beggining:
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


cacheSolve <- function(specialM, ...){

	## This function calculates the inverse of a 'special matrix' created
	## with the 'makeCacheMatrix' function. It first checks if the inverse
	## has already been calculated; if so, it gets the inverse from the 
	## cache and skips the computation. Otherwise, it calculates the inverse
	## matrix and sets the inverse matrix in the cache.

	## Check the inverse matrix within 'specialM'
	inverse <- specialM$getInverse()
	if(!is.null(inverse)){
		message("getting cached data")
		## Return the inverse matrix
		return(inverse)
	}	

	## Calculate the inverse matrix of the matrix within 'specialMatrix'
	matrix <- specialM$get()
	inverse <- solve(matrix)

	## Modify the special matrix to store the inverse
	specialM$setInverse(inverse)

	## Return the inverse matrix
	inverse
}