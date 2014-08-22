## The two functions below cache the inverse of a matrix. The first one makeCacheMatrix creates
##  a special "matrix", which is  a list containing a function to set and get values
## The second one cacheSolve calculates the inverse of the special "matrix" created by makeCacheMatrix.
## It gives back the inverse from the cache if it is already calculated and skips the computation;
## otherwise it calculates the inverse, caches it and gives it back.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
            inve <- NULL
        set <- function(y) {
                x <<- y
                inve <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inve <<- inverse
        getinverse <- function() inve
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


## This function calculates the inverse of the special "matrix" created by makeCacheMatrix

cacheSolve <- function(x, ...) {
          inve <- x$getinverse()
        if(!is.null(inve)) {
                message("getting cached data")
                return(inve)
        }
        data <- x$get()
        inve <- solve(data, ...)
        x$setinverse(inve)
        inve

}

