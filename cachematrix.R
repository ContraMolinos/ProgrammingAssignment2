## Put comments here that give an overall description of what your
## functions do

## This function will contain the functions to set and get both the
## matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # This will be the inverse of the matrix.
  inv_m<-NULL
  # Set the matrix.
  set<-function(y){
    x<<-y
    inv_m<<-NULL
  }
  
  # Get the value of the matrix.
  get<-function() x
  
  # Set the inverse of the matrix.
  set_inv<-function(inverse) inv_m<<-inverse
  
  # Get the inverse of the matrix.
  get_inv<-function() inv_m
  
  # List functions.
  list(get=get,set=set,set_inv=set_inv,get_inv=get_inv)
  
}


## This function will check if the inverse of the matrix has
## already been calculated. If not, it calculates and set it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_m<-x$get_inv()
  if(!is.null(inv_m)){
    message("Getting cached inverse.")
    return(inv_m)
  }
  data<-x$get()
  inv_m<-solve(data)
  x$set_inv(inv_m)
  inv_m
}
