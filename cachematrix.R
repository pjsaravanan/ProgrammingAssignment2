## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	# Initialize 
	inverseincache <- NULL
	matrixincache <- NULL


	setmatrix <- function ( y )
	{
		x <<- y 
		inverseincache <<- NULL
		matrixincache <<- y
	}

	getmatrix <- function () x

	setinversematrix <- function (solve) inverseincache <<-solve ## solve returns the inverse of matrix passed 

	getinversematrix <- function () inverseincache ## returns the valueincache

	list ( setmatrix=setmatrix, getmatrix=getmatrix, setinversematrix=setinversematrix, getinversematrix=getinversematrix )
	
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverseincache <- x$getinversematrix()
		print("matrix in cache")
		print(matrixincache)
		print("get matrix")
		print(x$getmatrix())
		if ( identical( matrixincache, x$getmatrix() ) && !is.null(inverseincache) )
		{ 
			message ( "getting cached data" )
			return(inverseincache)
		}
		else
		{
			sm<-x$getmatrix()
			message("performing inverse ")
			inverseincache<-solve(sm,...)
			matrixincache<<-sm
			x$setinversematrix(inverseincache)
			inverseincache
		}
}
