# These two functions take advantage of the <<- operator to
# cache a costly operation (in this case, inverting a matrix)

# Create a list containing a matrix along with
# a potentially cached inverse of that matrix
makeCacheMatrix <- function(current.matrix = matrix()) {
	
	# Start with an emply cache
    cached.inverse <- NULL
    
    set <- function(new.matrix) {
    	# Make the new matrix the current one
    	# and clear out the cache
        current.matrix <<- new.matrix
        cached.inverse <<- NULL
    }
    
    get <- function() {
        # Simply return the current matrix
        current.matrix
    }
    
    setinverse <- function(new.inverse) {
        # Store the passed matrix (presumably the
        # inverse of the current matrix) in the cache
        cached.inverse <<- new.inverse
    }
    
    getinverse <- function() {
        # Return whatever is in the cache...
        cached.inverse
    }
    
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# Return the inverse of a matrix created with
# makeCacheMatrix. If it has already been
# calculated, just return cached value.
cacheSolve <- function(x) {

   # First check the cache
   inv <- x$getinverse()

   # If there's no matrix in the cache,
   # calculate it now...
   if(is.null(inv)) {
        inv <- solve(x$get())
        # Be sure to cache the result
        x$setinverse(inv)
    } else {
        # No need to recalculate
        # Let the user know...
        message("getting cached data")
    }

	# Return the inverse
	inv
}
