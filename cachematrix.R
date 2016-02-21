## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function is a function which computes the inverse of a matrix
##Also this function set the values to environment i.e. in the Cache
makeCacheMatrix <- function(x = matrix()) {
  inver<-NULL
  set<-function(q1){
    x<<-q1
    inver<<-NULL
  }
  get <-function() x
  setinverse<- function(solve) inver <<- solve
  getinverse<- function() inver
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## Write a short comment describing this function
##This function will check if inverse already exists for this matrix
##If inverse exists, environment values is displayed else its calculated
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invers<-x$getinverse()
  if(!is.null(invers)){
    message("Getting Cached Data")
    return(invers)
  }
  data<-x$get()
  invers<-solve(data,...)
  x$setinverse(invers)
  invers
}