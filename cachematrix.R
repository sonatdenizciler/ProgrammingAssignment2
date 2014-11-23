## functions create a special list object that can cache 
## the inversion of a matrix with the matrix itself
## and they can return the cached inversion if available  

## the function creates a list of functions that can set, get the matrix
## and the inversion of the matrix if provided
makeCacheMatrix <- function(x = matrix()) {
  inversion <- NULL
  
  set <- function(newmatrix) {
      x <<- newmatrix
      inversion <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinversion <- function(solve) {
    inversion <<- solve
  }
  
  getinversion <- function() {
    inversion
  }
  
  list(set = set, get = get,
       setinversion = setinversion,
       getinversion = getinversion)
}


## the function returns the inversion of a matrix in the makeCacheMatrix
## if already exists else it will create the inversion and set it and 
## return the value 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversion <- x$getinversion()
  if(!is.null(inversion)) {
    message("getting cached inversion")
  } else {
    m <- x$get()
    inversion <- solve(m)
    x$setinversion(inversion)
    message("new inversion calculated")
  }
  inversion
}
