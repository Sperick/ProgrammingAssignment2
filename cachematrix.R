## Together these functons allow the calculation and caching of 
## the inverse of matrices.

## This function takes in a matrix object and creates a new
## object that allows the user to get and set the data 
## within this central matrix whilst also allowing the
## inverse of the matrix to be cached in memory.
makeCacheMatrix <- function(x = matrix()) {
        
        ## initialise the cache to be NULL/empty
        m <- NULL
        
        ## create getter and setter methods
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        
        ## output a list of getters and setters
        list(set = set, get =get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function works hand in hand with the makeCacheMatrix
## function above to return the inverse of a matrix wither
## by calculation or returning from the cache.
cacheSolve <- function(x, ...) {
        
        ## Searches cache for inverse value
        m <- x$getinverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # if cache is not found, calculate, set and return
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
