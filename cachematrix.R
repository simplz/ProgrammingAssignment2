## Course 2 Week 2 Assignment: 
## Caching the Inverse of a Matrix 

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        InvMx <- NULL # default value of InvMx
        
        get <- function () x # cache matrix x
        setInvM <- function(IMatrix) InvMx <<- IMatrix # store InvMx in cache
        getInvM <- function() InvMx # retrieve cached value of InvMx
        list(get=get, setInvM=setInvM, getInvM=getInvM)
        
}


## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        
## If InvMx is !is.null, cacheSolve will trigger getInvM() to retrieve its cached
## value. 
## Else InvMx is NULL, cacheSolve will inverse the matrix x using solve() and 
## cache the value of InvMx.
        
        InvMx <- x$getInvM()
        if(!is.null(InvMx)) {
                message("Getting cached data...")
                return(InvMx)
        } else{ 
                InvMx <- solve(x$get())
                x$setInvM(InvMx)
                return(InvMx)
                
        }
}