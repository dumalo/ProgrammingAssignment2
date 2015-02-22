## These functions are modifications of the example code 
## provided in Assignment 2. They will create an object that 
## stores a matrix and caches it's inverse.

## makeCacheMatrix() creates a special matrix which is 
## actually a list that contains functions to 1) set a 
## matrix value, 2) retrieve the matrix value,
## 3) set the matrix inverse, 4) get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve() will calculate the inverse of the special "matrix" 
## created with makeCacheMatrix(). But before doing so it checks to 
## see if the inverse has already been calculated. If yes, it gets
## the inverse from cache. If no, it calculates the inverse and sets 
## the value via the setinverse function in makeCacheMatrix().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
