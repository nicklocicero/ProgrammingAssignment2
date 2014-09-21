##  Creates an object from a matrix
makeCacheMatrix <- function(x = matrix()) {
        
        ## sets m to NULL
        m <- NULL
        
        ## sets 'set' to function that changes object's x value
        ## without having to create a new object, we can change x
        set <- function(y) {
                
                ## sets x to equal y one environment above which is
                ## our object's environment
                x <<- y
                m <<- NULL
        }
        
        ## sets get to function which gets the value of x
        get <- function() x
        
        ## setinverse function uses solved matrix as var in cache
        setinverse <- function(solve) m <<- solve
        
        ## getinverse function returns matrix inversion
        getinverse <- function() m
        
        ## creates list of variables equal to their original vals
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Using makeCacheMatrix object, takes functions and caches them within
## makeCacheMatrix object
cacheSolve <- function(x, ...) {
        
        ## sets m to value of x$getinverse()
        m <- x$getinverse()
        
        ## test m is not null
        if(!is.null(m)) {
                message("getting cached data")
        
                ## return cached inverse matrix and end function
                return(m)
        }
        
        ## set data to x, our matrix
        data <- x$get()
        
        ## solve for inverse of our matrix
        m <- solve(data, ...)
        
        ## set inverse into object, cache it
        x$setinverse(m)
        
        ## return cached inverse matrix, end function
        m
}
