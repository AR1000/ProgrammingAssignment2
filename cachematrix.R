## Assignment: Caching the Inverse of the Matrix
##
##The functions bellow are used to create a special 
##object that stores a matrix and cache's its inverse.    
## Assume that matrix supplied is always invertible. 


## This function, 'makeCacheMatrix' create a special "matrix" object,   
## which is really a list containing a function to
## 1.set the values of the Matrix
## 2.get the values of the Matrix
## 3.set the values of the Inverse
## 4.get the values of the Inverse

makeCacheMatrix <- function(x = matrix()) {
    m <-NULL
    set<- function (y){
        x <<- y
        m <<- NULL
    }
    get<-function()x
    setInv<-function(solve)m <<- solve
    getInv<- function () m
    list  (set=set, get=get, setInv=setInv, getInv=getInv)
    
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix (above). If the inverse has already been calculated, 
## then the cachesolver retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix 'm' that is the inverse of 'x'
    m<-x$getInv()
    if(!is.null(m)){
        message("Getting cached Matrix")
        return (m)
    }
    mat <- x$get()
    m<-solve(mat, ...)
    x$setInv(m)
    m
}
