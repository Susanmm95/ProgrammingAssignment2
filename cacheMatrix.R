## These functions 1) cache inverse matrix results
## 2) checks caache if inverse already calculated
## 3) calculates inverse if cached value needs to be updated

## This function creates a special matrix that is really a list to
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of matrix inverse
## 4) get the value of matrix inverse

makeCacheMatrix <- function(x = matrix()) {
	n <- NULL
	set <- function(y) {
		x <<- y
		n <<- NULL
	}
	get <- function () x
	setinverse <- function(solve) n <<- solve
	getinverse <- function () n
	list (set = set, get = get, setinverse = setinverse, 
		getinverse = getinverse)
}

## the following function calculates the value of the inverse of the matrix
## but first checks to see if inverse already exists in cache

cachesolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	n<-x$getinverse()
	## check to make sure inverse isn't already in cache
	if(!is.null(n)){
		message("getting cached data")
		return(n)
	}
	## otherwise calculae inverse
	data <- x$get()
	n<-solve(data, ...)
	x$setinverse(n)
	n
}
