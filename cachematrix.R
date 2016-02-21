## Instead calculate again the matrix inverse, we can use the previous calculation
## that was stored in different environment with the function environment


## save data in cache, not only the input data but also result data
## in this case the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve = matrix()) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Check whether there was a calculation before, if there was then use the previous 
## value, else do the calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

## How to test

## > invert <- matrix(c(1,2,-1,0), nrow=2, ncol=2)
## > test_cache <- makeCacheMatrix(invert)
## > test_cache$get()
## [,1] [,2]
## [1,]    1   -1
## [2,]    2    0
## > test_cache$getinverse()
## NULL
# > result_cache <- cacheSolve(makeCacheMatrix(invert))
# > result_cache
# [,1] [,2]
# [1,]    0  0.5
# [2,]   -1  0.5
# > result_cache <- cacheSolve(test_cache)
# > result_cache
# [,1] [,2]
# [1,]    0  0.5
# [2,]   -1  0.5
# > test_cache$setinverse(matrix(1:9,nrow=3,ncol=3)) ##random matrix
# > result_cache
# [,1] [,2]
# [1,]    0  0.5
# [2,]   -1  0.5
# > result_cache <- cacheSolve(test_cache) 
# getting cached data
# > result_cache
# [,1] [,2] [,3]
# [1,]    1    4    7
# [2,]    2    5    8
# [3,]    3    6    9