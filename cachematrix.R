## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  matrix_inv <- NULL
  set_matrix <- function(y) {
    ## Set X as new matrix passed
    x <<- y 
    ## Set inverse matrix as null, since it is new
    matrix_inv <<- NULL
  }
  get_matrix <- function() x
  set_inverse <- function(inverse) matrix_inv <<- inverse
  get_inverse <- function() matrix_inv
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get_matrix()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}
