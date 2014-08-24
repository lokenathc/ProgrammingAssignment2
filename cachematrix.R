## Following two functions illustrate caching results of expensive functions for reuse
## We are caching results of a matrix inversion

## The function makeCacheMatrix creates a list which contains a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function first checks if the inverse of the matrix
## has already been calculated. If so,it gets the inverse from the
## cache and skips inverse calculation. Else, it calculates the inverse
## and sets it to the cache.
## The function only works if an invertible matrix is passed as its parameter

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Getting inverse of matrix from cache")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}



