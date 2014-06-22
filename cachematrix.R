## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than computing it repeatedly. Here we will
## take advantage of the scoping rules of the R language in a pair of functions that 
## cache the inverse of a matrix.


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated, then cacheSolve retrieves the 
## inverse from cache.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("retrieving cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
