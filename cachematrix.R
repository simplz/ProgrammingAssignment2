## Course 2 Week 2 Assignment: 
## Caching the Inverse of a Matrix 

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
##
## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.
## 
## HOW TO USE:
## To test, use matrix(1:4, 2, 2),
## UserInput <- makeCacheMatrix(martix(1:4,2,2))
## cacheSolve(UserInput)
## cacheSolve() will return the inverse of the matrix, either from solve() 
## or its cached value (Getting cached data...).
##
## Disclaimer: I am a novice programmer. I did various online searches on platforms
## such as stackoverflow. I wrote/modified parts of this program such that it 
## was clear to my own understanding.
## 
## You may skip below comments. Go to the program >>>>>>>>>>>>>>>
## 
## My reflection
## I learnt many new things. 
## Example 
## (1) how cache works using <<-
## (2) how to use $ on x matrix, instead of x$getInvM, use x$getInvM()
## (3) solve() to return inverse of the matrix
## (4) avoid test matrix such as matrix(1:9,3,3) because its det is '0' and 
##     R will return error. 
## (5) try another test matrix such as matrix(rnorm(9),3,3). It works!
##
## Have fun! Practice makes perfect. Keep trying!


makeCacheMatrix <- function(x = matrix()) {
        
        InvMx <- NULL # default value of InvMx
        
        get <- function () x # cache matrix x
        setInvM <- function(IMatrix) InvMx <<- IMatrix # store InvMx in cache
        getInvM <- function() InvMx # retrieve cached value of InvMx
        list(get=get, setInvM=setInvM, getInvM=getInvM)
        
}


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