## Put comments here that give an overall description of what your
## functions do
## The first function creates a special "matrix" object that can 
## cache its inverse. The second function computes the inverse of 
## the special "matrix" returned by makeCacheMatrix. 

## Write a short comment describing this function
## Store four functions in the main function makeCacheMatrix.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## Check if the inverse matrix has been calculated and return it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}


