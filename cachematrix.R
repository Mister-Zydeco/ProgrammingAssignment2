## Put comments here that give an overall description of what your
## functions do

## Creates special object holding a numeric matrix (assumed always to
## be invertible) and its cached inverse. The cached inverse is null
## until it is computed with the companion function "cacheSolve" below.
## 

makeCacheMatrix <- function(x = matrix()) {
    cachedInvers <- NULL
    set <- function(mxToInvert) {
       x <<- mxToInvert
       cachedInvers <<- NULL
    }
    get <- function() x
    setCachedInvers <- function(calculatedInvers) {
      cachedInvers <<- calculatedInvers
    }
    getCachedInvers <- function() cachedInvers
    list(set=set, get=get,
      setCachedInvers=setCachedInvers,
      getCachedInvers=getCachedInvers)
}


##  cacheSolve takes as argument(s) special objects as created by makeCacheMatrix.
## The first time that cacheSolve is called, it will calculate and cache the inverse
## with the object, and then return the inverse. On subsequent calls with the same
## object as an argument, it will simply return the cached inverse and avoid a
## potentially expensive matrix inverse calculation.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix thatis the inverse of 'x'
  invers <- x$getCachedInvers()
  print(invers)
  if (!is.null(invers)) {
    message("Found and returning cached inverse")
    return(invers)
  }
  mxToInvert <- x$get()
  invers <- solve(mxToInvert,...)
  x$setCachedInvers(invers)
  invers
}
