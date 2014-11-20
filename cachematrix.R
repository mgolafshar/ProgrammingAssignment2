## The purpose of the following functions are to get the inverse of 
## a matrix and cache it so it can be looked up in the cache rather 
## than having to be recomputed should it be needed again.

## ------------------------------------------------------------------ 

## The first function, makeCacheMatrix, creates a special "matrix' to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the inverse of the matrix
## 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
    }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)

}

## The following function, cacheSolve, computes the inverse of the special "matrix" created 
## with the makeCacheMatrix function. It first checks to see if the inverse has 
## already been computed. If so, it gets the inverse matrix from the cache and skips 
## the computation. Otherwise, it computes the inverse of the data and sets the inverse 
## calculation in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}