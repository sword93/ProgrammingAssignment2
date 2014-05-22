## Put comments here that give an overall description of what your
## functions do

##  This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setMatrixInversion <- function(inversion) i <<- inversion
  getMatrixInversion <- function() i
  list(set = set, get = get,
       setMatrixInversion = setMatrixInversion,
       getMatrixInversion = getMatrixInversion)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
  i <- x$getMatrixInversion()
  if(!is.null(i)) {
    message("getting cached inversion of matrix ")
    return(i)
  }
  mat <- x$get()
  d <- det(mat)
  if (d == 0 ) {
    print ("Determinant is 0, Inversion of Matrix don't exists !!!")
  } else {
    i <- solve(mat, ...)
    x$setMatrixInversion(i)
    i
  }
}

## function Test 1  
mat1 <- matrix(c(1,2,3,4,5,4,3,2,1), ncol=3)
cmat1 <- makeCacheMatrix(mat1)
cacheSolve(cmat1);
cmat1$getMatrixInversion()

## function Test 2 (determinant = 0)
mat2 <- matrix(c(1:9), ncol=3)
cmat2 <- makeCacheMatrix(mat2)
cacheSolve(cmat2)
cmat2$getMatrixInversion()


