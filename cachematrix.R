## The functions below are designed to cache the inverse of a given square invertible matrix

## mackeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){ ##set the value of the matrix
    x<<-y
    inv<<-NULL
  }
  get<-function()x ##get the value of the matrix
  setinv<-function(inverse) inv<<-inverse ##set the inverse
  getinv<-function() inv ##get the inverse
  list(set=set, get=get, setinv=setinv,getinv=getinv) 
}


## cacheSolve function computes the inverse of the special matrix returned by makeCacheMatrix function. In case the inverse of the matrix has already been calculeted, computaion of the inverse is skipped.

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv         
}
