## Returns a list of four functions to cache and manage a matrix and its inverse:
## set - set the matrix
## get - get the cached matrix
## setInverse - set the inverse matrix
## getInverse - get the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Computes the inverse of the matrix x, and caches the result in the structure 
## defined with the function makeCacheMatrix

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
    }
    else {
        data <- x$get()
        
        ## Computes a matrix that is the inverse of 'x' (the cached matrix)
        inv <- solve(data, ...)
        
        ## Caches the computed inverse matrix
        x$setInverse(inv)
    }
    inv
}

