# Catching the inverse of a matrix

# This first function creates a special "matrix" object that catches the inverse of a matrix.
# The list created contains a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function computes the inverse of the matrix returned by makeCacheMatrix(). 
# The matrix is always assumed to be invertible.
# If the inverse has already been calculated. 
# it prints a message and retrieves the result from the cache.
# If not, it computes the inverse and sets the value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    inv <- solve(x$get())
    x$setinverse(inv)
    inv
}