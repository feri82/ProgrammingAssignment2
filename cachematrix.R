## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The following 2 functions allow to calculate the inverse of a matrix which 
## is subsequenly stored in cache

## This function returns a list of 4 functions:
## - for setting/getting values of a matrix
## - for setting/getting the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {

  inverse<-NULL
  
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function() x
  
  setInverse<-function(inv) {inverse<<-inv}
  getInverse<-function() {inverse}
  
  list(get = get, set = set, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

## This function calculates the inverse of a matrix created 
## by the "makeCacheMatrix" function
## If the inverse of a matrix already exists, then this function gets the inverse 
## from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse<-x$getInverse()
    if(!is.null(inverse)) {
      inverse
    } else {
      ## calculating the inverse
      data<-x$get()
      inverse<-solve(data, ...)
      x$setInverse(inverse)
      inverse
    }
}
