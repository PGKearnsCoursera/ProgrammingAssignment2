# makeCacheMatrix functions similar to makeVector
# It returns a list made up of functions revolving around the given matrix
# cacheSolve then uses those functions to determine if a matrix needs to be inverted
# or if the inverse already exists in memory, then returns the inverse
#NOTE : THESE FUNCTIONS ASSUME A Valid Invertible MATRIX IS SUPPLIED

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  #set establishes the cached values of the matrix and space for the inverse
  set <- function(y = matrix()) {
    x <<- y
    inverse <<- NULL
  }
  #get returns the value of the matrix
  get <- function() x
  #set caches a matrix value in memory(specificaly the inverse) 
  setinverse <- function(solve) inverse <<- solve
  #getinverse returns the value of the inverse of the matrix
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#cacheSolve takes a list object, and uses the functions within that object
#to calculate and store the inverse of a matrix
cacheSolve <- function(x, ...) {

  inverse <- x$getinverse()
  if (!is.null(inverse)){
    message("Cache exists, retreiving")
    ## Return a matrix that is the inverse of 'x$get'
    return (inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  ## Return a matrix that is the inverse of 'x$get'
  inverse
}



