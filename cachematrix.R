## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Functions that cache the inverse of a matrix
##
## Usage example:
##
## > source('cachematrix.R')
## > m <- makeCacheMatrix(matrix(c(1, 2, 3, 4), c(2, 2)))
## > cacheSolve(m)
##      [,1] [,2]
## [1,]  -2  1.5
## [2,]   1 -0.5

## Create a special "matrix", which is a list containing
## a function to
##   - set the value of the matrix : set
##   - get the value of the matrix : get
##   - set the value of the inverse matrix : seti
##   - get the value of the inverse matrix : geti


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(
    set = set,
    get = get,
    seti = setinverse,
    geti = getinverse
  )  
}


## Write a short comment describing this function
## > cacheSolve(m)
## take the inverse of m from the cache if there exists one
## or calcute the inverse if it isn' t already done

cacheSolve <- function(x, ...) {
  ## Return a matrix i that is the inverse of 'x'
  i <- x$geti()
  if(!is.null(i)) {
    message("getting cached inversed matrix")
    return(i) ## take the inverse from the cache if there existes one
  }
  ## calcute the inverse if it isn' t already done
  i <- solve(x$get(), ...)
  x$seti(i)
  i
}
