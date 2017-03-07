## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a matrix as an argument and 
## returns a list.
## This list provides a number of accessor methods
## (get, set, setinverse, getinverse) for interacting
## with the matrix.
## In addition to this the matrix inverse is stored
## to reduce the calculation overhead in calculating
## the inverse each time this method is called.

makeCacheMatrix <- function(x = matrix()) {

    # initialise inverse value to null
    i <- NULL
    
    # set new value for matrix and clear existing inverse value
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x

    setinverse <- function(inverse) i <<- inverse

    getinverse <- function() i

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes a makeCacheMatrix as an argument and
## returns the matrix inverse.
## If the matrix inverse has not previously been calculated
## the inverse is calculated, otherwise the cached value
## is returned.

cacheSolve <- function(x, ...) {
    
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
