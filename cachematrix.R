##Calculates the inverse of a square matrix by turning into into a "CacheMatrix" function and
##then using first the solve function and second loading
##the result if it was already calculated

## This function creates a matrix that is a list of functions that keep the inverse matrix result
##if it exists, otherwise it returns a NULL value

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


##Calculates the invertex matrix unless it was already calculated, in which case
##it loads the result already in cache

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv        
}
