## LUCIA FRANCHESCA DELA CRUZ

makeCacheMatrix <- function(x = matrix()) { ## THIS IS WHAT IS UTILIZED TO CREATE CACHE
  inv <- NULL ## AN EMP MTRX
  get <- function() x ## THE METHOD OF OBTAINING THE FUNC
  set <- function(y) { ## SET 
    x <<- y ##INVRS MTRX
    inv <<- NULL ##FOR THE U INVRSE MTRX
  }
  getinv <- function() inv ## LAY THE FUNC
  setinv <- function(inverse) inv <<- inverse
  list(get=get, set=set, getinv=getinv, setinv=setinv) ##MTRX OF THE MTRX
}
cacheSolve <- function(x, ...) { ## CALCULATION OF THE INVRSE MTRX
  inv <- x$getinv() ##FUNC FOR THE INVRS MTRX
  if (!is.null(inv)) {
    message("inverse is cached") ## OUTCOME
    return(inv) ## RESTORE TO THE INVRS
  }
  m <- x$get() ## CONVEY INVRS
  inv <- solve(m, ...) ## DETERMINE THE MTRX
  x$setinv(inv)
  return(inv) ## REINSTATE TO THE INVRS FUNC
}
## BY UTILIZING THE RSTUDIO PROGRAM, THIS IS EMPLOYED TO CALCULATE FOR THE INVRS MTRX