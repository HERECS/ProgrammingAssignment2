## This R file contains two functions.

## makeCacheMatrix is a function that cache the inverse 
## of a specified matrix. To illustrate this function, take an example
## on a matrix, matrix(c(1,2,2,1),2,2). 
## A <- makeCacheMatrix(matrix(c(1,2,2,1),2,2)) gives a list
## that store matrix A and its inverse(if have been computed)

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}

	get<- function() x
	
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv

	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve is a function that compute the inverse of a matrix.
## If its inverse has been computed, it will return 
## message "getting cached data". If otherwise, 
## it compute the inverse and cache it in makeCacheMatrix.
## This function must be used only after the use of makeCacheMatrix
## To illustrate this function, back to the previous example 
## matrix(c(1,2,2,1),2,2). 
## If we want the inverse of matrix(c(1,2,2,1),2,2),
## we need to store this matrix in makeCacheMatrix by
## A <- makeCacheMatrix(matrix(c(1,2,2,1),2,2)),
## then obtain its inverse by
## cacheSolve(A).
## If A does not cache its inverse, then cacheSolve will
## compute its inverse and cache it in A.
## Else, cacheSolve(A) return the inverse from A
## and a message "getting cached data"


cacheSolve <- function(x, ...) {

	inv <- x$getinverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}

	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv

}
