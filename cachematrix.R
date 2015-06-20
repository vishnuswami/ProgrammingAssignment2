## I have desccribed two main functions -makeCacheMatrix and cacheSolve. The former consists of 4 sub functions which are described below


## This function takes the matrix, which is assumed to be invertible, as its one and only input.
## It has 4 subfunctions: set(y)-sets the value of matrix to be worked on, get()-obtains the matrix from memory when called,
## setinverse(inverse)-sets inverse matrix as "inverse", getinverse()- retreives the value of the inverse from memory.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache. If the inverse hasn't been calculated
## ,the function calculates in using the solve(x) function and stores the value in the cache for future retreival.  

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
