## mackeCacheMatrix() creates a matrix objet and cahches it.
## It also caches the inverse of a matrix and returns it on being called.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
        setinverse = setinverse,getinverse = getinverse)
}

## cacheSolve() looks for a cached inverse and on finding it returns it.
## Otherwise, it calculates the inverse of matrix and caches it for further
## use.

cacheSolve <- function(x, ...) {
    #Get cached inverse. If found, return it.
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    #Cached inverse not found. So calculate it and cache it.
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
