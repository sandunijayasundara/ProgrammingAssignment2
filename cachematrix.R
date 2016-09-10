## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }

  get<-function() x
  setInv_Mat<-function(inverse) inv<<- inverse
  getInv_Mat<-function() inv
  list(set=set, get=get,
       setInv_Mat=setInv_Mat,
       getInv_Mat=getInv_Mat)
}
## Above function create the matrix and below function returns the inverse of created matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInv_Mat()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix<-x$get()
  inv<-solve(matrix, ...)
  x$setInv_Mat(inv)
  inv
}
