## This function creates a matrix
## It also exposes functions for setting and retrieving the matrix data
## Lastly, it exposes function for accessing and setting the inverse 
## of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        ## Sets the matrix data to operate on
        set <- function(y) {
                x <<- y
                inv <- NULL
        }
        
        ## Retrieves the matrix data to operate on
        get <- function() x
        
        ## Sets the inverse of the matrix
        setinv <- function(a_inv) inv <<- a_inv
        
        ## Retrieves the inverse of the matrix
        getinv <- function() inv
        
        ## Makes all the above functions accessible for without this function
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function calculates the inverse of the specified matrix
## It first attempts to retrieve the matrix inverse from cache
## If it doesn't find the inverse in the cache, it calculates 
## then persists it to the cache for future retrieval
cacheSolve <- function(x, ...) {
        
        ## Retrieve the matrix inverse from the cache
        c_inv <- x$getinv()
        
        ## Check if the inverse of the matrix was already set
        ## If it was already set, return from this function
        if(!is.null(c_inv)) {
                message("getiing cached inverse...")
                return(c_inv)
        }
        
        ## If the inverse of the matrix was not set yet, 
        ## calculate and persist it to cache
        data <- x$get()
        c_inv <- solve(data, ...)
        x$setinv(c_inv)
        
        ## Return the calculated matrix inverse
        c_inv
}