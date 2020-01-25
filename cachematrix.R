## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL					# initialize inverse property
	set <- function(y){				# create set function for matrix object
		x <<- y
		inverse <<- NULL
	}
	get <- function() x				# return matrix argument
	setInverse <- function(input) inverse <<- input	# anonymous function to set inverse property 
	getInverse <- function() inverse		# return inverse property
	list(	set = set,				# return list with defined functions to make 'special' matrix
     		get = get,
		setInverse = setInverse,
		getInverse = getInverse )		
}

## X is a "special" matrix containing functions from makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getInverse()			# try to retrieve inputs inverse
	if(!is.null(i)){			# if there is an inverse, return it
		message("Return cached data")
		return(i)
	}
	data <- x$get()				# else, get compute inverse and return
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

