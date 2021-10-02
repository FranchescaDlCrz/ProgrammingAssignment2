makeCacheMatrix <- function(x = matrix()) {##cACHE
  +     inv <- NULL ##THE ZERO MATRIX
  +     get <- function() x ##THE FUNCTION
  +     set <- function(y) {
    +         x <<- y
    +         inv <<- NULL ##THE INVERSE MATRIX
    +     }
  +     getinv <- function() inv ##SET THE FUNCTION AS
  +     setinv <- function(inverse) inv <<- inverse
  +     list(get=get, set=set, getinv=getinv, setinv=setinv)
  +     ##MTX
      + }
cacheSolve <- function(x, ...) { ##SOLVING FOT THE INVERSE
  
  +     inv <- x$getinv() ##THIS IS THE FUNCTION FOR INVERSE
  +     if (!is.null(inv)) {
    +         message("inverse is cached") ##THE RESULT
    +         return(inv) ##RETURN TO THE INVERSE MATRIX
    +     }
  +     m <- x$get()
  +     inv <- solve(m, ...) ##SOLVE FOR THE MATRIX
  +     x$setinv(inv)
  +     return(inv) ##RETURN TO THE INVERSE FUNCTION
  + }
##THIS IS USED FOR SOLVING INVERSE MATRIX USING RSTUDIO