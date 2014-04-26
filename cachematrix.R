## makeCacheMatrix and cacheSolve allows cached calculation of a matrix's inverse
## the matrices used must be invertable

## makeCacheMatrix wraps a plain matrix into a list which can be used with cacheSolve
## the original matrix can be extracted by calling the 'get' element of the list

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve takes a list created by makeCacheMatrix and returns
## the inverse of the matrix, either the cached value if it's already calculated
## otherwise calculates and caches it
## the first argument has to be a list created with makeCacheMatrix,
## additional arguments of the solve() function can be passed along

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
