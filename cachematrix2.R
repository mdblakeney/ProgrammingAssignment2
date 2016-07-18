## makeCacheMatrix and cacheSolve, together, compute the inverse of a matrix.
## To save cycles, once the inverse of a matrix has been calculated once, the
## inverse is stored along with the original matrix. Repeated calls to solve the
## matrix, therefore, return the cached value of the inverse, rather than 
## repeating the calculation.

## makecacheMatrix takes a (square) matrix as input, and outputs an "enhanced" version 
## of the matrix, which stores (a) the matrix itself and (b) a cached copy of the 
## input matrix's inverse. When initialized, the inverse is NOT stored (set to NULL)
## but is only calculated when asked to do so by cacheSolve function below.

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


## cacheSolve takes as input an "enhanced" matrix as defined by the makeCacheMatrix 
## function above. Outputs the (possibly cached) inverse of the matrix. First checks 
## to see if there is a value stored for the inverse. If not (if the call for the
## inverse returns NULL) then cacheSolve calculates the inverse and stores it by
## calling setinverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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
