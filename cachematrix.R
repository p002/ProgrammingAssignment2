## This script caches time-consuming inverse matrix computations

## makeCacheMatrix creates a special "matrix", which is 
## really a list containing a function to 
## * set the value of the matrix
## * get the value of the matrix
## * set the value of the inverse matrix
## * get the value of the inverse matrix

makeCacheMatrix <- function( x = matrix() ) {
        
        # initialize inverse matrix to null
        invmatrix = NULL
        
        # to set value of the matrix 
        set <- function(y) {               
                x <<- y                
                invmatrix <<- NULL
        }
        
        # to get value of the matrix
        get <- function() x
        
        # to set value of the inverse of matrix
        setinverse <- function(solve) invmatrix <<- solve
        
        # to get value of the inverse matrix
        getinverse <- function() invmatrix

        list( set = set, get = get, 
			setinverse = setinverse, 
			getinverse = getinverse)
        
}


## cacheSolve function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and 
## sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function( x, ... ) {
        
        # get value of the inverse
        invmatrix <- x$getinverse()
        
        #check if inverse is already calculated
        if( !is.null( invmatrix ) ) {
                message("getting cached data")
                return( invmatrix )
        }
        
        #otherwise calculates inverse of data
        data <- x$get()
        invmatrix <- solve(data, ...)
        x$setinverse(invmatrix)
        invmatrix
}
