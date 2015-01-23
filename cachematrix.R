## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  a<-NULL
  set<-function(b) {
    x<<-b
    a<<-NULL
  }
  get<-function() x
  setinv<-function(solve) a <<- solve
  getinv<-function() a
  list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## Write a short comment describing this function

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
