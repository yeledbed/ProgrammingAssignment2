## K Simpson
## May 22, 2015
## R Programming: Progamming Assignment 2
## makeCacheMatrix and cacheSolve work in conjuciton to create
## a matrix object to cache a matrix's inverse and return the 
## the inverse.


## Takes an invertible matrix and returns a matrix "object"
## with a cached inverse

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setInverse <- function(solve) i <<- solve
     getInverse <- function() i
     ## return a list of functions for cacheSOlve to perform
     list(set=set,get=get,
          setInverse=setInverse,
          getInverse=getInverse)
}


## Takes matrix "object" made by makeCacheMatrix function
## and returns its inverse (from the cache if it exists)

cacheSolve <- function(x, ...) {
     i<-x$getInverse()
     ## Check if matrix inverse already exists
     ## If it does, retrieve and return it from cache
     if(!is.null(i)) {
          message("getting cached inverse")
          return(i)
     }
     ## Otherwise calculate the inverse
     Matrix <- x$get()
     i <- solve(Matrix)
     ## Cache the inverse
     x$setInverse(i)
     ## Return a matrix that is the inverse of 'x'
     i
}
