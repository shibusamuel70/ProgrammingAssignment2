## Programming Assignment 2: Lexical Scoping
## Following two functions creates and stores a matrix vector and cache's its inverse

## makeCacheMatrix function accepts a matrix vector and does the following functionality
## sets & gets the value of the vector and sets & gets the inverse of the matrix 

makeCacheMatrix <- function(mat = matrix()) {
  m <- NULL
  set <- function(y) {
    mat <<- y
    m <<- NULL
  }
  get <- function() mat
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve function produces the inverse of the matrix. The inverse of matrix if already calculated it gets from the cache and ## avoids recalculating the inverse

cacheSolve <- function(mat, ...) {
  m <- mat$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- mat$get()
  m <- solve(data, ...)
  mat$setinverse(m)
  m
}