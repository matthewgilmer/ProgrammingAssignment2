## The first function, makeCacheMatrix, creates a "special" matrix and does 4 things:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the inverse value of the matrix
## 4. gets the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The second function, cacheSolve, does 2 things:
## 1. checks to see if there is a cached value for the inverse of the matrix...
## 2. if there is, it prints the cached value; if there is not, it computes the inverse of the matrix and then prints the value

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
