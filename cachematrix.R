## R Programming, week 3: Assignement
## script by Jan Grade

## The function makeCacheMatrix creates a "matrix" which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## The function cacheSolve computes the inverse of the "matrix"
## If the inverse has already been calculated,
## cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}