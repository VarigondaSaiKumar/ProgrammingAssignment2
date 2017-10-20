## Calculates the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(var){
    x <<- var
    inverse <<- NULL
  }
  get <- function() x
  setInv <- function(mat) inverse <<- mat
  getInv <- function() inverse
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInv()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  mat <- x$get()
  det_val <- det(mat)
  if(det_val == 0) {
    print("Determinant is Zero. Inverse of a matrix doesn't exist")
    x$setInv(0)
  }
  else {
  inverse <- solve(mat)
  x$setInv(inverse)
  }
  print(inverse)
}
