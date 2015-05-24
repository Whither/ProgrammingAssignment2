## These two functions create a matrix object for caching

## makes a matrix object for caching

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) inverse <<- solve
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## retrieves the cached the matrix object from makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
