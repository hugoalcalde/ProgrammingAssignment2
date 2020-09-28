## Trying to find a way of reducing time while computing 
## the inverse of a matrix 

## This function creates a special type of matrix with
## cache. It will be useful for our final purpose. 

makeCacheMatrix <- function(mymatrix = matrix()) {
  invertedmatrix <- NULL
  set <- function(y) {
    mymatrix <<- y
    invertedmatrix <<- NULL
  }
  get <- function() mymatrix
  set.inverse <- function(solve) invertedmatrix <<- solve
  get.inverse <- function() invertedmatrix
  list(set = set, 
       get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}



## This function calculates the inverse of a matrix
## Only if the inverse was not calculated before. 

cacheSolve <- function(x, ...) {
  m <- x$get.inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$set.inv(m)
  m

}
