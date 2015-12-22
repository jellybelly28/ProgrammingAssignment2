##Part 1: The makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## 1) set the value of the matrix
## 2) get the value of the marix
## 3) solve the inverse of the matrix
## 4) inverse the matrix

makeCacheMatrix <- function(x = matrix()) {
  ##x will be a matrix e.g. matrix(c(4,5,6,7), nrow=2, nrow=3)
  inv <- NULL
  ## instead of using m in the example, i'm going to use the variable, "inv"
  ## resets inv to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
    ##1) set the value of the matrix
    ## 'superassigns' y to x
    ## resets inv to NULL with 'superassigns' operator
  }
  get <- function()
    x
  ##2) gets the value of the matrix
 
  setinverse <- function(solve)
    inv <<- solve
  #3) set the value of the inverse
  
  getinverse <- function()
    inv
  #4) gets the value of the matrix
  
  list(
    set = set, get = get, setinverse = setinverse, getinverse = getinverse
  )
  ##lists the functions
}


##Part 2: cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been caclualted (and the matrix has not changed)
## then the cache solve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of matrix 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached inverted matrix")
    return (inv)
    ##if inv is already cached, then the message, "getting cached inverted matrix" will appear
    #the inverse matrix, 'inv' will be returned
  }
 
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  ##otherwise, we will be computing the inverse fo the matrix
  ##print the inverted matrix
}
