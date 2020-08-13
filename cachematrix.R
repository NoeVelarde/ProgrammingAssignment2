## Put comments here that give an overall description of what your
## functions do: This funtion creates an object "Matrix and returs its invers.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y){
                x<<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
##This function calculates the inverse of a Matrix, if the matrix is already calculated
## it send a message "getting cached data"


cachesolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
