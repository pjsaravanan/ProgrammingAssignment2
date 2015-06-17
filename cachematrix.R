## A list of four functions for setting & getting - matrix and it's inverse
## Argument - a valid square invertible matrix 
## Test results captured and available at the bottom of document
makeCacheMatrix <- function(x = matrix()) 
{
	## variable to store the matrix inverse - initialized to NULL
	mtx <- NULL 
	## set the matrix
	## Argument : a valid square invertible matrix 
	set <- function(y) 
	{
		## Check whether the available matrix and passed matrix are same
		## identical(obj1,obj2) returns a logical value if both are same
		if ( !identical(x,y) ) 
		{
			## if it is not same, replace with the current matrix 
			x <<- y
			## reset the inverse matrix to NULL
			mtx <<- NULL 
		}
	}
	## get the matrix
	## returns the matrix
	get <- function() x
	## set the inverse matrix
	## solve performs the inverse of matrix and stores in mtx
	setinv <- function(solve) mtx <<- solve
	## get the matrix inverse
	getinv <- function() mtx
	## list for the four functions 
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve checks whether the matrix inverse is avaialble 
## and returns if available, else performs the inverse of matrix
cacheSolve <- function(x, ...) 
{
	## Return a matrix that is the inverse of 'x'
	## get the inverse matrix 
	mtx <- x$getinv()
	if(!is.null(mtx)) 
	{
		## if mtx is not null, return the mtx as it is
		message("getting cached data")
		return(mtx)
	}
	## following statements executed only when mtx is null 
	## this actually is a else part of the previous if statement
	
	## get the matrix for which matrix inverse is to be performed
	data <- x$get()
	## perform the inverse matrix and store in mtx
	mtx <- solve(data, ...)
	## update the inverse matrix
	x$setinv(mtx)
	## return the inverse matrix
	mtx
}
## Tests Results
## > rm(list=ls())                      ## Remove all objects
## > source("cachematrix.R")            ## Load Source
## > a<-makeCacheMatrix(matrix(1:4,2,2))## Prepare Cache, test with matrix
## > cacheSolve(a)                      ## Calculation Success
##    [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(a)                      ## Matrix exists, inverse from cache
## getting cached data
##    [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > a$set(matrix(4:7,2,2))             ## Change the value of matrix
## > cacheSolve(a)                      ## Cache Cleared, Cache Updated
##    [,1] [,2]
## [1,] -3.5    3
## [2,]  2.5   -2
## > cacheSolve(a)                      ## Matrix exists, inverse from cache
## getting cached data
##    [,1] [,2]
## [1,] -3.5    3
## [2,]  2.5   -2