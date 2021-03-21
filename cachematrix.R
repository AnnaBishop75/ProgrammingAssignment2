## The aim of storing and retrieving information in/from a cache is to reduce computing time.
## Two functions, "makeCacheMatrix" and "cacheSolve" are given and described below.

## "makeCacheMatrix" is a function that outputs a special vector (list) of four functions that:
## 1) set the values in a matrix and store them in matrix "x"
## 2) retrieve matrix "x"
## 3) compute the inverse of the matrix and store it as matrix "inv" in a cache
## 4) retrieve the matrix "inv" from the cache.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y) {
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setinverse<-function(solve) inv<<-solve
        getinverse<-function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## "cacheSolve" is a function that outputs the inverse of a matrix, "x", created with "makeCacheMatrix" above.
## It takes as input the output from "makeCacheMatrix".
## It first checks to see if the inverse is stored in the cache, and retrieves it (skipping the calculation).
## Otherwise it computes the inverse of the matrix, stores it in the cache and outputs the inverse.

cacheSolve <- function(x, ...) {
        ## If the cache is not empty, return the inverse matrix from the cache.
        inv<-x$getinverse()
        if(!is.null(inv)) {
                message("getting inverse from the cache")
                return(inv)
        }
        ## If the cache is empty, compute the inverse of the matrix.
        data<-x$get()
        inv<-solve(data)
        x$setinverse(inv)
        inv
}
