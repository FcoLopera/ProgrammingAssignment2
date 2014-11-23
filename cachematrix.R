## These functions calculate the inverse of a matrix and store the result in a cache


## Matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

	## Initialize the inverse property
    i <- NULL

    ## Set the matrix
    set <- function( matrix ) {
			## Save the new matrix
            m <<- matrix
			## Set the inverse to null to recalculate if it is necessary
            i <<- NULL
    }

    ## Get the matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## Set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Return a list of methods for this object
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the matrix if the inverse has not already been calculated.
## If not, returns the stored value of the inverse. 
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Return the inverse if has it
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

	## If not, we need to calculate the inverse
	
    ## First, get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(m)

    ## Return the matrix
    m
}