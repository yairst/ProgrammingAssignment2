## The function cacheSolve returns the inverse of the matrix created with
# the makeCacheMatrix function.
# If the cached inverse is available, cacheSolve retrieves it, and if
# not, it computes, caches, and returns it.


## makeCacheMatrix creates a special "matrix",
# which is really a list containing a functions to
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the inverse matrix of the matrix
# 4.get the inverse matrix of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv_x <<- inverse
    getinverse <- function() inv_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## cacheSolve calculates the inverse matrix of the special "matrix"
# created with the above makeCacheMatrix function.
# It will first check to see if the inverse has already been calculated. If so, it gets
# the inverse from the cache and skips the computation. Otherwise, it calculates the inverse
# of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getinverse()
    if(!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
    }
    data <- x$get()
    inv_x <- solve(data)
    x$setinverse(inv_x)
    inv_x
}
