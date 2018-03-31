## Functions which will create a list object
## used to hold the value of a matrix and 
## retrieving a cached value of its inverse

## This method will create a list which can
## which holds the contents of a matrix
## along with functions set or get a 
## cached value of its inverse

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


## This method will return the cached inverse
## of a matrix stored in the list form generated
## by the makeVector function.  If the inverse
## has not yet been computed, it will compute
## the inverse and store it in the list.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("retrieving cached inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
