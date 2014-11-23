##  makeCacheMatrix creates a special "matrix", 
##  which is really a list containing a function to:
##  1- set the value of the matrix.
##  2- get the value of the matrix.
##  3- set the value of the inverse.
##  4- get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse (solve) of the special "matrix" 
## created with the above function. However, it first checks to see if 
## the inverse has already been calculated.  If so, it gets the inverse 
## from the cache and skips the computation.
## Otherwise, it calculates the inverse (solve()) of the data and sets 
## the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached inverse data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}

