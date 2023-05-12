## Put comments here that give an overall description of what your
## functions do

# R program to find inverse of a Matrix

# Create a matrix
# Create 3 different vectors
# using combine method.
a1 <- c(3, 2, 5)
a2 <- c(2, 3, 2)
a3 <- c(5, 2, 4)

# bind the three vectors into a matrix 
A <- rbind(a1, a2, a3)

# print the original matrix
print(A)

# Use the solve() function 
# to calculate the inverse.
T1 <- solve(A)

# print the inverse of the matrix.
print(T1)


## makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## CacheSolve function

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}