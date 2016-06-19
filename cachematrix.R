## Put comments here that give an overall description of what your
## functions do

## Make the matrix to cache 

makeCacheMatrix = function(x=matrix())
{
  inv = NULL
  set<- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse<- function(inverse) inv<<- inverse
  getInverse<- function() inv 
  list(set=set,get=get,setinverse=setInverse,getinverse=getInverse)
}


##Cache the matrix on re-run

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
