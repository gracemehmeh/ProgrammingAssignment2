# Assignment: Caching the Inverse of a Matrix
# Assume that the matrix supplied is always invertible.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}
    setInv <- function(inverse) {inv <<- inverse}
    getInv <- function() {inv}
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data (inverse matrix)")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...) #Computing the inverse of a square matrix can be done with the solve function in R. 
    x$setInv(inv)
    inv
}
