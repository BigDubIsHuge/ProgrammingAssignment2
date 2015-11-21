## The following functions can be used to cache the inverse matrix.
## Calculating the inverse matrix can be computational expensive, especially
## when the input matrix is very large.
## The basic idea is to store the inverse matrix when it is calculated at the first time,
## and use the store values when the inverse matrix is required under the condition
## that the input matrix is not changed.


## The makeCacheMatrix function is a list of four functions
## to get the matrix, 
## to get the inverse matrix,
## to set the matrix,
## and to set the inverse matrix.
## It is noted that these functions are not used if the input matrix is a singular or non-square matrix.
makeCacheMatrix <- function (x=matrix()){
		if(ncol(x)!=nrow(x)){
		print("Please enter a sqaure matrix, and then run the makeCacheMatrix function again")
		return()
		}else	if(det(x)==0){
		print("Please noted that the matrix is singular, please enter a valid matrix")
		return
		}

		im <- NULL
		set <- function(y=matrix()){
			x <<- y
			im <<- NULL
	}
	get <- function() {x}
	setinverse <- function(inverse_matrix) {im <<- inverse_matrix}
	getinverse <- function() {im}
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## The following function calculates the inverse of the matrix created with the function above.
## The cached inverse matrix, if there is one, will be used, and no additional calculations are required.
## Otherwise, the inverse matrix will be calculated and stored as a cached inverse matrix.
## It needs to note that the cached inverse matrix becomes NULL if the input matrix is changed.
cacheSolve <- function(x, ...){
	im <- x$getinverse()
	if(!is.null(im)){
		message("getting cached inverse")
		return(im)
	}
	data <- x$get()
	im <- solve(data,...)
	x$setinverse(im)
	im
}
