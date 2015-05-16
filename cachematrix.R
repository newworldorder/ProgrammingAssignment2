## makeCacheMatrix - constructs an "object" with set, get, set_inverse, and get_inverse 
##                   as methods
## cacheSolve - computes an inverse matrix only if there isn't one saved already 

## Construct an object that will represent a matrix and its inverse
## 
makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(new_matrix){
    x <<- new_matrix
    inverse_matrix <<- NULL
  }
  get <- function() x
  set_inverse <- function(i_matrix) inverse_matrix <<- i_matrix
  get_inverse <- function() inverse_matrix 
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## Checks for saved inverse matrix, 
## if present use it, otherwise compute it
##
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()
  if(!is.null(m)){
    message("using cached version")
    return(m)
  }
  d <- x$get()
  m <- solve(d)
  x$set_inverse(m)
  m
}
