# The purpose of the 2 function is to cache potentially time-consuming
# computations. For example, taking the mean of a numeric vector is typically a
# fast operation. However, for a very long vector, it may take too long to 
# compute the mean, especially if it has to be computed repeatedly (e.g. in a
# loop). If the contents of a vector are not changing, it may make sense to
# cache the value of the mean so that when we need it again, it can be looked up
# in the cache rather than recomputed. In this Programming Assignment you will
# take advantage of the scoping rules of the R language and how they can be
# manipulated to preserve state inside of an R object.


# The 1st function, `makeCacheMatrix` creates a special "vector", which is
# really a list containing a function to
# 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse
#
# To use it, first have to pass a square matrix to the function
# Sample
# a <- matrix(c(4,3,3,2), ncol=2)
# x <- makeCacheMatrix(a)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

 # The 2nd function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setsolve`
# function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

# Sample
# a <- matrix(c(4,3,3,2), ncol=2)
# x <- makeCacheMatrix(a)
# cacheSolve(x)
