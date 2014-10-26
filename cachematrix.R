## The makeCacheMatrix() and cacheSolve() functions work together to provide a way to calculate
## the inverse of a matrix and cache the result so that it can be retrieved as needed without requiring 
## additional calculations of the inverse.

## The makeCacheMatrix() function takes a matrix as its input creates a list of four functions. Set() 
## sets the matrix from its input to be the new matrix in the parent environment. Get() returns the matrix 
## from its unput. Setinverse() assigns the matrix from its input to a variable in the parent environment.
## This function is meant to be called by the cacheSolve() function, not directly. Getinverse() returns 
## the inverse matrix stored in a variable in the parent environment or returns NULL if no inverse matrix 
## is stored. 

makeCacheMatrix <- function(x = matrix()) {       ## recieves matrix x as input
  m <- NULL                                 ## assigns value NULL to local variable m.      
  set <- function(y) {                      ## sets a new matrix to x and NULL to m in parent environment
    x <<- y                                 ## assigns matrix from input to x in parent environment
    m <<- NULL                              ## assigns value NULL to m in parent environment; local m unchanged
  } 
  get <- function() x                       ## returns the matrix from its input
  setinverse <- function(inverse) m <<- inverse   ## assigns inverse matrix from its input to m in parent environ 
  getinverse <- function() m                ## returns the value of variable m from parent environment
  list(set = set, get = get,                ## a list of the names assigned to each function
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve() function takes a makeCacheMatrix() object as its input and returns the inverse of the matrix
## pointed to by makeCacheMatrix().If the inverse has already been caluclated and stored, cacheSolve() will return
## that matrix thereby saving a calculation. If the value of the variable which stores the inverse is NULL
## cacheSolve() will calculate the inverse, store it in the containing environment, and return it.

cacheSolve <- function(x, ...) {            ## recieves a makeCacheMatrix object as an argument
  m <- x$getinverse()                   ## gets value of m from containing environ, stores it in local variable m
  if (!is.null(m)) {                    ## checks to see if value of m is not NULL
    message("getting inverse matrix")   ## if value is not NULL, displays a message 
    return(m)                      ## returns the inverse matrix stored in variable m in the containing envirnment
  }
  data <- x$get()                       ## gets matrix pointed to by makeCacheMatrix, assigns it to variable data
  m <- solve(data)                      ## computes inverse of data and assigns it to local variable m
  x$setinverse(m)             ## takes local variable m, an inverse matrix, assigns it to m in containing environment
  m                           ## returns the inverse matrix    
}
