## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  M <- x
  invM <- NULL
  push <- function(y){
    M <<- y
    InvM <<- NULL
  }
  pop <- function() M
  pushInv <- function(m) invM <<- m
  popInv <- function() invM
  
  list(pop = pop, push = push, popInv = popInv, pushInv = pushInv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invData <- x$popInv()
  if (!is.null(invData)){
    message('Getting data from cache')
    return(invData)
  } else {
    message('Setting data to cache')
    x$pushInv(solve(x$pop()))
    x$popInv()
  }

}