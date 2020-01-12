## Put comments here that give an overall description of what your
## functions do

## a special matrix that caches its inverse and 
# returns a list containing four functions to set and get the value of the
# matrix and to set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # function to set the value of the matrix. It also clears the old
        # inverse from the cache
        set <- function(y) {
                x <<- y    # Set value
                m <<- NULL # Clear cache
        }
        # function to get the value of the matrix
        get <- function() x
        # function to set the inverse. 
        setInverse <- function(inverse) m <<- inverse
        # function to get the inverse
        getInverse <- function() m
        
        # Return a list with the above four functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x) {
        m <- x$getInverse() # cached value for the inverse
        if(!is.null(m)) { # when cache is not empty, returns it
                message("getting cached data")
                return(m)
        }
        # The cache was empty. We need to calculate it, cache it, and then return it.
        data <- x$get()  # Get value of matrix
        m <- solve(data) # Calculate inverse
        x$setInverse(m)  # Cache the result
        m                # Return the inverse
}
