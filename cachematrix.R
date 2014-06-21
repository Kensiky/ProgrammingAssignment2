##########################################################################
## Computing the inverse of a matrix can take a long time. The following
## function can cache a function and store its inverse. So instead of 
## computing the inverse again we will check to see if the inverse is in
## cache.  If it is in the cache we will pull the value from the cache. If
## it is not in the cache we will computer the inverse.
##      (Code is a modification of sample code from R. Peng, R Programming
##       offered from coursera)
##########################################################################

#######################################################################
##       ## Example of running makeCacheMatrix() and cacheSolve(A)
## Create A, a list of four functions set, get,setsolve,getsolve
##
## A <- makeCacheMatrix()

## Use A's set function to create the Matrix
##  2 3
##  4 1
## 
## A$set(matrix(c(2, 4, 3, 1), nrow=2, ncol=2) )

## Use A's get function to retrieve the created Matrix
##
## A$get()

## Solve the inverse of the matrix in A
##
## cacheSolve(A)

## Call cacheSolve again to retrieve the inverse from cache.
##
## cacheSolve(A)
#######################################################################


#######################################################################
## makeCacheMatrix Creates 4 function
## set() Will store a matrix
## get() Will return a stored matrix
## setsolve() Will store an inverse matrix
## getsolve() Will return an inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
########################################################################

########################################################################
## cacheSolve with find the inverse of a matrix.
## It will first check if the inverse is in cache to return the value.
## If the inverse is not in the cache it will be compute, stored in cache
## then that value will be returned

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
########################################################################
