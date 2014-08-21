## creates a matrix object with cacheable inverse
## solving for inverse of the matrix returns cached value if matrix is unchanged
## 
## the script assumes given matrix is invertible
## errors are not handled


## makeCacheMatrix creates a matrix object with cacheable inverse
## 
##  sample usage:
##  
##  creates cacheable inverse matrix 'x':
##      x <- makeCacheMatrix()
##
##  sets the "value" of the matrix:
##      x$set(value)
##
##  x$setInverse(inv) and x$getInverse() sets and gets the inverse of the
##      define matrix respectively, both functions are used by cacheSolve()
##      use cacheSolve(x) to get the inverse of the matrix

makeCacheMatrix <- function(num = matrix()) {
    inverse <- NULL
    set <- function(y) {
        num <<- y
        ## value is redefined, clear the cache
        inverse <<- NULL
    }
    get <- function() {
        num
    }
    setInverse <- function(inv) {
        inverse <<- inv
    }
    getInverse <- function() {
        inverse
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## returns the inverse of the matrix
## if there is no cached inverse the function solves for it

cacheSolve <- function(x, ...) {
    ## get inverse as stored by matrix object
    inv <- x$getInverse()
    ## if cached inverse exists, return cached data
    if(!is.null(inv)) {
        message("matrix unchanged: getting cached data")
        return(inv)
    }
    ## get the value of the matrix
    num <- x$get()
    ## solve for the inverse
    inv <- solve(num, ...)
    ## set the value of the inverse matrix to be put in the cache
    x$setInverse(inv)
    inv
}