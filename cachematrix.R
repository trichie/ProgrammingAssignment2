## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # when initializing, the inverse Matrix is set to NULL
  invMat <- NULL  
  # inside the MakeMatrix object, 4 functions are defined
  # setting the original matrix and clearing the inversed Matrix, i.e. setting it as NULL
  setMatrix <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  # getting the original matrix
  getMatrix <- function() x
  # setting the inverted matrix
  setInverseMatrix <- function(inv) invMat <<- inv
  # getting the inverted matrix
  getInverseMatrix <- function() invMat
  # defining a list of elements through which the functions are accessible "outside", e.g. in cacheInv
  list(set = setMatrix, get = getMatrix, setinv = setInverseMatrix, getinv = getInverseMatrix)
}




## Write a short comment describing this function

cacheSolve <- function(x) {
  # inverse Matrix is loaded
  invMat <- x$getinv()
  # if the inverse Matrix has already been calculated (i.e. cacheSolve has run before and the matrix hasn't been changed since)...
  if(!is.null(invMat)) {
    message("getting cached data")
    # ... the cached matrix is returned
    return(invMat)
  }
  # Otherwise, the original Matrix is read, inverted and the inverse is returned
  invMat <- solve(x$get())
  x$setinv(invMat)
  return(invMat)
}


# Small test case
mat <- matrix(nrow=2,c(1,3,2,4))
testMat <- makeMatrix(mat)
invMat <- cacheinv(testMat)
invMat
round(mat %*% invMat,15)
makeCacheMatrix <- function(x = matrix()) {

}
