## The first function should take in any matrix
## use get and set functions for retrieving the matrix
## and also retrieving the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <<- NULL
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
}

## This function uses 'solve' to get the inverse since
## the assumption is that the given matrix is invertible
## Note: the use of caching to get the value of matrix if the inverse exists
## in the cache 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}
