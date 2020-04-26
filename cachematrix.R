## Below are two functions that cache the inverse of matrix

## This function create a special matrix, that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inv = NULL
set = function(y){
  x <<- y
  inv <<- NULL
}
 get <- function()x
 setinv = function(inverse)
   inv <<- inverse
 getinv = function() inv
 list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of special matrix 

cacheSolve <- function(x, ...) {
        ## Return makeCacheMatrix that is the inverse
  inv = x$getinv()
  if(!is.null(inv)){
    return(inv)
  }
  data = x$get()
  inv = solve(data, ...)
  x$setinv(inv)
  return(inv)
}
