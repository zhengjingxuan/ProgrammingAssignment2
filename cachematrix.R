## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix consitsts of set,get,setinv,getinv
## library(MASS) used to calculate inverse 
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL    #innitialzing inverse as NULL
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  get <- function()x  #get matrix X
  setinv <- function(inverse)inv <<- inverse
  getinv <- function(){
    inver<-ginv(x)
    inver%*%x  #obtain inverse of the matrix
  }
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}
## Write a short comment describing this function
cacheSolve <- function(x, ...)   #get cache data
{
  inv <- x$getinv()
  if(!is.null(inv)) {    #check if its NULL
    message("getting cached data!")
    return(inv)  #return the inverse value
  }
  data <- x$get()
  inv <- solve(data,...)    #calculates inverse value
  x$setinv(inv)
  inv  ## Return a matrix that is the inverse of 'x'
}


