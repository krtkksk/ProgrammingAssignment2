##caching the inverse of the matrix

##calculatig the invrse of matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


##returning the inverse of matrix
##matrix stored in cache

cacheSolve <- function(x, ...) {
  
  
        ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
matrix1<-makeCacheMatrix(matrix(2:5,2,2))
matrix1$get()
matrix1$getInverse()
cacheSolve(matrix1)
