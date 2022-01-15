## The goal of this respository is to write two functions that cache
## the inverse of the matrix. The first function creates a list, and
## the second function calculates the inverse of the matrix in case
## it was not calculated (and stores it in the cache), otherwise it
## will get the inverse of the matrix from the cache.

## This first function creates a special matrix (a list) containing
## a function to set the value of the matrix, get the value of the
## matrix, set the value of the inverse of the matrix and get the
## value of the inverse of the matrix respectively.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse)(inv<<-inverse)
  getinverse<-function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This second function will check if the inverse of the matrix was already
## calculated, in case yes, it will get the inverse from the cache, otherwise it
## will compute the inverse of the matrix and will store it in the cache.

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setinverse(inv)
  inv
}
