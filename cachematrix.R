## Calculating the inverse of a large matrix can be computationally expensive.
## These functions store the inverse of a matrix  in the parent 
## environment of the function once calculated, and allow the retrieval 
## of the saved inverse if available in the parent environment.

## makeCacheMatrix creates a matrix and provides functions to 
## retrieve the matrix and its inverse, and to overwrite the 
## matrix and the calculated inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve rerieves a previously caclulated matrix inverse, calculating 
## it if no cached solution exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
