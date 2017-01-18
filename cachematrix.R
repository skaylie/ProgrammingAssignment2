## The makeCacheMatrix function creates a special "matrix", which really is a 
## list of functions to set and get the matrix and to set and get its inverse.  
## cacheSolve first checks for a cached inverse matrix and then calculates
## it if none exists.

## Returns a list of functions to manage a cached matrix and its inverse.  It
## only sets a new matrix and clears the cache if the new matrix being set is
## not identical (ie the matrix hasn't changed).

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                if(!identical(x, y)){
                        x <<- y
                        inv <<- NULL
                }
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Returns the inverse of a matrix from makeCacheMatrix by first checking if the
## matrix's inverse has been cached, and then calculating it and 
## caching it if it has not been.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
