## The function returns a list of functions that acts on the vector (matrix)


makeCacheMatrix <- function(x = matrix()) {
  # set matrix
  inverse<-NULL
  set <- function(m)
  {
    
      x<<-m
      inverse<<-NULL
  }
  
  get<-function() m
  setinverse<-function(inv) inverse<<-inv
  getinverse<-function() inverse
  list(set=set,get=get,getinverse=getinverse,setinverse=setinverse)
  
}



## Function checks if the inverse is computed. If the inverse is already computed the same is returned , else inverse is computed.
## Also the computed inverse is saved (cached) for future reference

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse<-x$getinverse()
  if(!is.null(inverse))
  {
    message("Getting data from cache")
    return(inverse)
  }
  inverse<-solve(x$get()%*%x$get())
  x$setinverse(inverse)
  inverse
  }