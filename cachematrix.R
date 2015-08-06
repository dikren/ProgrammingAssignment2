## Functions here are for caching inverse value of an invertible matrix
## which allows fast reuse of the stored value computed earlier
## instead of computing this value every time it is needed

## This function stores an input matrix, which inverted form is needed,
## and caches the inverse value of the specified matrix.
## Output of this function is an interface (a list of functions) which
## allows to get/set the matrix to be inverted and the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## This function gets the interface returned by the makeCacheMatrix 
## and returns the inverse matrix

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
