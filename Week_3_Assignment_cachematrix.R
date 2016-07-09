## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
get <- function() x
setsolve <- function(solve) s <<- solve
getsolve <- function() s
list(set = set, get = get,
     setsolve = setsolve,
     getsolve = getsolve)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("cached data")
    return(s)
  }
data <- x$get()
s <- solve(data, ...)
x$setsolve(s)
s
}

makeCacheMatrix <- function(x = matrix()){    
  +  m <- NULL
  +  set <- function(y){
    +    x <<- y  
    +    m <<- NULL 
    +  }
  +  get <- function() x 
  +  setInverse <- function(solve) m<<- solve 
  +  getInverse <- function() m 
  +  list(set = set, get = get,
          +       setInverse = setInverse,
          +       getInverse = getInverse)  
}

cacheSolve <- function(x, ...) {
  -            +  m <- x$getInverse()                 #query the x matrix's cache
    +  if(!is.null(m)){                    #if there is a cache the inverse has been previously calculated
      +    message("getting cached data")    # sent message indicating this is just cache 
      +    return(m)                         # return the cache  
      +  }
    +  data <- x$get()                     # get the matrix used by makeCacheMatrix function 
    +  m <- solve(data, ...)               # calculate the inverse of the matrix
    +  x$setInverse(m)                     # store the inverse matrix in cache using the makeCacheMatrix set function
}