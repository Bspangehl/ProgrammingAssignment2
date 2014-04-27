## Together makeCacheMatrix and cacheSolve make it possible to cache the inverse
## of a matrix.
## This functionality can be used in order to save the costly computation of
## repeatedly calculating the inverse of a matrix.
## The first time cacheSolve is called for a particular matrix it will calculate
## its inverse as well as cache this value for futher use.
## As long as the matrix does not change, this cached inverse will be recalled
## when needed (i.e. no inverse re-calculation of the same matrix is done)
## If the matrix changes or an inverse for a new matrix is required cacheSolve
## will calculate the inverse for the new matrix and cache this new value for
## further use.


## makeCacheMatrix creates a special vector that is able to cache the inverse
## of a matrix.
## Use $get() and $getinverse() to view the matrix and its inverse
## Usage: 
## x <- makeCacheMatrix(matrix(c(5,6,7,8),nrow=2))

makeCacheMatrix <- function(x = matrix()) {
             ## Create a vector that is able to cache the inverse of a matrix

    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve will either compute the inverse of the matrix returned by
## makeCacheMatrix or if available will retrieve the cached inverse of the matrix.
## If the inverse has already been calculated and the matrix has not changed
## then cacheSolve retrieves the inverse from the cache.
## Usage:
## cacheSolve(x)

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
  
}
