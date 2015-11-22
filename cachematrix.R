## The following functions allow for the caching of a matrix for further operations and 
## also for the calculation and caching of the inverse of the matrix initially passed. 

## The makeCacheMatrix function does nothing but holds a matrix initially passed
## to, returns it when requested and does the same for the inverse of the same martix.
## The function returns a list of the actual functions that are called.

makeCacheMatrix <- function(x = matrix()) {
  M <- x ## The Matrix
  invM <- NULL ## The Inverse of the Matrix
  
  ## Saves the Matrix
  push <- function(y){
    M <<- y
    InvM <<- NULL
  }
  pop <- function() M ## Returns the Matrix
  pushInv <- function(m) invM <<- m ## Saves the Inverse
  popInv <- function() invM  ## Returns the Inverse
  
  list(pop = pop, push = push, popInv = popInv, pushInv = pushInv)

}


## The cacheSolve function firstly checks to see if the Inverse
## of the Martix exists. If it does it returns the Inverse.
## If not, it returns the value of the Matrix, calculates the Inverse
### using solve(), pushes it to the cache function and then returns the inverse

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