##  makeCacheMatrix() function takes a matrix as an input and produces a list of 4 functions

makeCacheMatrix <- function(x = matrix()) {  # input x will be a matrix, assumed to be square
  m <- NULL    #  m will be our inverted matrix and it's reset to NULL every 
  #    time makeCacheMatrix is called
  
  #  note these next four functions are defined but not run when makeCacheMatrix is called.
  #   instead, they will be used by cacheSolve() to get values for x or for
  #   m (solve) and for setting m.
  
  set <- function(y) {    # takes an input matrix
    x <<- y         # saves the input matrix 
    m <<- NULL      # resets the solved matrix to NULL
  }
  
  get <- function() { x }   # this function returns the value of the original matrix
  
  setsolve <- function(solve)  { m <<- solve }
  # this is called by cacheSolve() during the first cacheSolve()
  #  access and it will store the value using superassignment
  
  getsolve <- function() { m } # this will return the cached value to cacheSolve() on
  #  subsequent accesses
  
  list(get = get,          #  This list of internal functions accessed each time makeCacheMatrix() is called       
       set = set,
       setsolve = setsolve,  
       getsolve = getsolve)  
 
}

## cacheSolve() function takes the output list of the makeCacheMatrix() function 
## and either computes the inverted matrix or looks it up from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()               # accesses the object 'x' and gets the value of the solved matrix
  if(!is.null(m)) {              # if solved matrix was already cached (not NULL) ...
    message("getting cached data")  # ... send this message to the console
    return(m)                       # ... and return the solved matrix ... 
  }
  data <- x$get()        # we reach this code only if x$getsolve() returned NULL
  m <- solve(data, ...)   # if m was NULL then we have to calculate the solved matrix
  x$setsolve(m)           # store the calculated mean value in x (see setsolve() in makeCacheMatrix
  m               # return the solved matrix to the code that called this function
  
}
