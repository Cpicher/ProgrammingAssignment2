makeCacheMatrix <- function(x = matrix()) {
        ## Initializes the result matrix with NULL
	m <- NULL
	## Defines a function to set the new matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
	## Returns the maxtrix just set above 
        get <- function() x
	## sets the the inverted matrix
        setInverse <- function(solve) m <<- solve
        ## Returns the inverted matrix just set above
	getInverse <- function() m
	## returnes the special matrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
        ## Checks if the inverted matrix has been already calculated
	m <- x$getInverse()
	## if yes returns the inverted matrix cached by makeCacheMatric function
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
	## if not calculate the inverted matrix and displays it
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}