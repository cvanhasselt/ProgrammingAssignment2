## 
##
## Coursera R Programming Course Week 3 programming assignment
## by Chris van Hasselt

## The makeCacheMatrix() function decorates a normal R matrix, adding
## functions to the data matrix to allow for caching.  The cacheSolve() 
## function uses the decorated matrix created by makeCacheMatrix(),
## either returning the inverse matrix from cache, or computing the inverse
## and storing the result inverse matrix in cache.

## 
## makeCacheMatrix() Function
##
## The makeCacheMatrix() function decorates a normal R matrix, adding
## functions to handle caching.  Although it is assumed that data that 
## the matrix is invertible, I've written the function to check if the 
## dataMatrix parameter is a matrix, and that it is square.

makeCacheMatrix <- function(dataMatrix = matrix()) {
    
    ## First check if the parameter is a matrix, and square.
    if (is.matrix(dataMatrix) & dim(dataMatrix)[1] == dim(dataMatrix)[2] ) {
        
        invMatrix <- NULL
        
        ## set, getData, setInvMatrix, and getInvMatrix functions add 
        ## functionality to 'decorate' the supplied dataMatrix.  The
        ## additional functions provide the framework for caching.
        set <- function (matrixInstance) {
            dataMatrix <<- matrixInstance
            invMatrix  <<- NULL
        }
            
        getData <- function() { 
            dataMatrix
        }
        
        setInvMatrix <- function (invMat) {
            invMatrix <<- invMat
        }
        
        getInvMatrix <- function () invMatrix
        
        ## Return the list of additional functions.
        list( set = setInvMatrix, 
              get = getData, 
              setInvMatrix = setInvMatrix,
              getInvMatrix = getInvMatrix)
    } else {
        # report error.
        print("makeCacheMatrix parameter is not a square matrix.")
    }

}

##
## cacheSolve() function
##
## The cacheSolve function depends on an object that
## defined via the makeCacheMatrix object that decorates
## a normal R matrix.  The cacheSolve function either returns
## a previously computed inverse matrix, or it computes a new 
## matrix inverse, using the R solve function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## 'x' is an object generated via the makeCacheMatrix function.
    invMatrix <- x$getInvMatrix()
    if(!is.null(invMatrix)) {
        ## return cached data here
        message("getting cached data")
        return(invMatrix)
    }
    ## ...otherwise, if the not cached, solve the matrix and 
    ## store it in cache.
    dataMatrix <- x$get()
    invMatrix <- solve(dataMatrix)
    x$setInvMatrix(invMatrix)
    invMatrix
}

## Simple testing example completed in the REPL

# A3 <- matrix(c(1,0,5,2,1,6,3,4,0),3,3)
# A3
# [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    0    1    4
# [3,]    5    6    0
# > B3 <- makeCacheMatrix(A3)
# > cacheSolve(B3)
# [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1
# > cacheSolve(B3)
# getting cached data
# [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1
# > 
