
# makeCacheMatrix is a function that stores a matrix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInverse   get the cached value (inverse of the matrix)
# * getInverse     get the cached value (inverse of the matrix)
#

makeCacheMatrix <- function(x = matrix()) {
  #Initializing the inverse property to null
  i <- NULL
  
  # set the matrix
  set <- function(b) {
    x <<- b
    i <<- NULL
  }
  
  #get the matrix
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

# CacheSolve function calculates the inverse of a the matrix created with 
# makeCacheMatrix with the assumption that matrix supplied is always invertible

cacheSolve <- function(x, ...) {
  
  # get the cached value
  i <- x$getinverse()
  
  # return the cached value if it exists
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  s <- x$get()
  i <- solve(s, ...)
  x$setinverse(i)
  i
}


