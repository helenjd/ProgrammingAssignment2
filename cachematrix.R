makeCacheMatrix <- function(x = matrix()) {
        #This function creates a "list" containing methods to get 
        #and set the values of the matrix and its inverse
        
        # initialise inv
        inv <- NULL
        #define set as a function using superassignment operator
        #so that x and inv can be used outside this environment
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        #define setinv using superassignment operator
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        ## Put all "sets" and "gets" into a list and return that
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                # if the inverse has already been calculated use the value from the cache
                message("getting cached data")
                return(inv)
        }
    # Need to get the matrix and invert it using "solve"
    to_be_inverted <- x$get()
    inv <- solve(to_be_inverted)
    # Put inverse into the cache for future use
    x$setinv(inv)
    inv
}