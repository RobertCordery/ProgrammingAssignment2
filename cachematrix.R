## Coursera Programming Assignment 2: Caching the inverse of a matrix
# The functions makeCacheMatrix() and cacheSolve() construct a matrix object 
# using makeCacheMatrix() that caches the matrix inverse the first time it is 
# calculated using cacheSolve(). 


## makeCacheMatrix makes a matrix object that can cache the matrix inverse 
# The inverse is cached in the parent environment the first time it is 
# calculated.  

makeCacheMatrix <- function(x = matrix()) {
    xi <- NULL
    set <- function(y) {
        x <<- y
        xi <<- NULL
    }
    get <- function() x
    setinv <- function(xinv) xi <<- xinv
    getinv <- function() xi
    list(set = set, get = get, setinv = setinv, getinv = getinv)
} # makeCacheMatrix


## cacheSolve takes the output Y <- makeCacheMatrix(y) and returns the matrix
# inverse of y. The first time it uses solve() to calculate the inverse and 
# caches the inverse in the environemnt of Y. Subsequently the cached value is 
# returned. 

cacheSolve <- function(x, ...) {
    
    xi <- x$getinv()
    if(!is.null(xi)) {
        message("getting cached inverse")
        return(xi)
    } # get cached inverse
    else{
        data <- x$get()
        xi <- solve(data, ...)
        x$setinv(xi)
    } # solve and save cached inverse
    # Return a matrix that is the inverse of 'x'
    xi
} # cacheSolve

