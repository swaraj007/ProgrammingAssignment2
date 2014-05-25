## This method creates a matrix whose inverse can be cached

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set= set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This method calculates the inverse of an invertible matrix.
##If it has already been calculated the method
## returns the stored inverse value from the cache

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if (!is.null(inverse)){
                message("getting inverse from cache")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
