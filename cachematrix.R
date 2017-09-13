## This entire file caches a matrix's inverse that can be inverted

## This function intializes a matrix while also being able 
## to set the matrix to different values. Has four functions in 
## it set, get, setinverse, and getinverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse 
        getinverse <- function() i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This checks to see if the matrix already has a cached inverse,
## if not then it computes the inverse and caches it. 
## If so then it returns the cached inverse.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("returning cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i}
