##  This first set finds the inverse of the matrix & caches the value so we can retrieve it later on

## Saving/caching matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
    }
    get <- function() {x}
    setInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This second set is used to actually get the inverse of the matrix saved/cached in first set

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("obtaining cached data")
                return(inv)
    }
    mat <- x$get()
    inv <- solve(mat,...)
    x$setInverse(inv)
    inv
}
