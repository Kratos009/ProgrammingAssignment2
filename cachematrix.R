## Below are the two funcitions that are used to create a special object that stores a matrix and cache's its inverse

## The first function makeCacheMatrix creates a special "matrix" , which is really a list containing functions to
## 1. set the value of the matrix.
## 2. get the value of the matrix.
## 3. set the value of the inverse.
## 4. get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { x <<- y;  m <<- NULL}
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse )
}


## The following function calculates the inverse of the special "matrix" created with the above method.
## However, it first checks whether the inverse has already been calculated.     
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) return(m)
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
