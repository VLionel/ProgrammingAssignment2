## These function provide a better performance to compute
## a matrix inversion. These functions benefit from
## caching the inverse of a matrix, rather than
## computing it repeatedly.


## This functions takes a square invertible matrix 
## that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y) {
                  x <<- y
                  m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)
  
}


## This function computes the inverse of the matrix returned by `makeCacheMatrix`. 
## If the inverse has already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            m <- x$getinv()
            if(!is.null(m)) {
              message("getting cached data")
              return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setsolve(m)
            m
  
}
