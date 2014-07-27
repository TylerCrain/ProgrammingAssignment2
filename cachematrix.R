## This function will create a matrix and clear the cache, setting it to null.
## In this function you can set your input, and get the inverse once it has been
## cached in the next function. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function will compute and Cache the matrix so it can be called
## later, and will not have to be recalculated after the inital call
## and calculation. If the matrix has already been solved, it will retrieve it
## from the cache

cacheSolve <- function(x, ...) {
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
