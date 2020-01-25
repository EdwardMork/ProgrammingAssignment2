## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y){
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(input) inverse <<- input
	getInverse <- function() inverse
	list(	set = set,
     		get = get,
		setInverse = setInverse,
		getInverse = getInverse )		
}

## X is a "special" matrix containing functions from makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getInverse()
	if(!is.null(i)){
		message("Return cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setInverse(i)
	i
}


## Testing

mat <- matrix(c(10,11,12,13),2,2)
print(mat)
testMat <- makeCacheMatrix(mat)
cacheSolve(testMat)

#Should print 'Return cached data'
cacheSolve(testMat)

