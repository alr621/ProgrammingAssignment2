## This function creates an R object that
## Initializes a variable 'a'
## Provides function get() to obtain 'raw' matrix
## Provides function setinv() to assign computed inverse matrix (of x) to a
## Provides function getinv() to obtain the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {

  a<-NULL
  set<-function(b) {
    x<<-b
    a<<-NULL
  }
  get<-function() x
  setinv<-function(solve) a <<- solve
  getinv<-function() a
  ##return a list of functions as an R object
  list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## This function does the actual inversing of matrix x
## It first checks if the inverse matrix has been found
## If yes, return result and quit
## If not, the inverse of x is calculated, saved to cached, and returned

cacheSolve <- function(x, ...) {
  a<-x$getinv()
  if(!is.null(a)) {
    message("Found cached data.")
    return(a)
  }
  else {
    message("No cached data found. Calculating inverse matrix.")
    data<-x$get()
    a<-solve(data)
    x$setinv(a)
    return(a)
  }
}
