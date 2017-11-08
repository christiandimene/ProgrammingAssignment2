## This function compute the inverse of a square matrix. However, it look in the cache to find if
## the inverse have already being calculated. if yes, it print the reseut. If no, it compute the
## inverse and cache it.

## This first function create a special "matrix" which is actualy a list of function to get and set
## the vector, and then to set and cache the inverse, and get the inverse of any squre inversible
##matrix x

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function compute the inverse of the special matix created by the function makeCacheMatrix
## it first look in the cache to find if
## the inverse have already being calculated. if yes, it print the reseut. If no, it compute the
## inverse and cache it.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
