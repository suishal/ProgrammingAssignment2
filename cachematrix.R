## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        set.inverse <- function(inverse) i <<- inverse
        get.inverse <- function() i
        list(set = set, get = get,
             set.inverse = set.inverse,
             get.inverse = get.inverse) 
        
}


## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$get.inverse()
        if(!is.null(i)) {
                message("getting inverse matrix")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$set.inverse(i)
        i
}
