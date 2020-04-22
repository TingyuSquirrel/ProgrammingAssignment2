## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function()m
  list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)

}


## This function first creates a matrix. The "set" function lets the user to reset the matrix 
## as well as the calculated inverse matrix. "get" function lets the user to retrieve the matrix created. 
##"setinverse" function defines what becomes the inverse matrix."getinverse" function retrieves the 
## calculated inverse matrix.


cacheSolve <- function(x, ...) {
    m<-x$getinverse()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setinverse(m)
    m
}
## The function checks if the inverse matrix is null or not. If it is not null, then the function
## returns the calculated and cached inverse matrix. If there is no cached inverse matrix, 
## the function calculates the inverse matrix and returns it. 




