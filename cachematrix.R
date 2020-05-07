## makeCacheMatrix is a function that saves a special matrix and its inverse in the cache,
## so that it can be rapidly accessed instead of calculating it repeatedly

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse_) inv <<- inverse_ 
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function computes the inverse of the given matrix. 
## If the inverse has already been calculated before, the cached inverse is returned

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  x$setinv(inv)
  inv
}

mat1 <- matrix(c(1,2,4,5), nrow = 2, ncol = 2, TRUE)
cmat <- makeCacheMatrix(mat1)
inverse1 <- cacheSolve(cmat)
inverse1
