## This file contains two functions:
## 1) makeCacheMatrix
## this function creates a list object including set/get 
## functions of a matrix and its inverse.
## 2) CacheSolve 
## it uses previous list object as input, checks whether the matrix's inverse 
## is in cache. If no, it will computer the inverse and store it in cache 


## makeCacheMatrix function creates a special "matrix" 
## object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # iv is the matrix inverse
  iv <- NULL
  # set x,iv
  set <- function(y) {
    x <<- y
    re <<- NULL
  }
  #return x
  get <- function() x
  setiv <- function(ivv) iv <<- ivv
  getiv <- function() iv
  ##return the list contains the set/get functions
  ##for matrix and its inverse 
  list(set = set, get = get,
       setiv = setiv,
       getiv = getiv)
}



## cacheSolve function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  iv <- x$getiv()
  ## checks if inverse in cache
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  ## computes inverse
  iv <- solve(data,...)
  x$setiv(iv)
  iv
}
