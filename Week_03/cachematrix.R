## These two functions are wrapping calls for inverse matrix so that
## complex calculations happen only first time and are cached later


## makeCacheMatrix creates a list of functions that work with this
## special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverseMatrix <<- inverse
    getinverse <- function() inverseMatrix
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" and if the inverse
## has already been calculated it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getinverse()
    if (!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    matrix <- x$get()
    inverseMatrix <- solve(matrix, ...)
    x$setinverse(inverseMatrix)
    
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix
}
