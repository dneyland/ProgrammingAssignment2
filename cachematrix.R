# Taking the inverse of a numeric matrix is typically a fast
# operation. However, for a very large matrix, it may take too long to
# compute the inverse, especially if it has to be computed repeatedly (e.g. 
# in a loop). If the contents of a matrix are not changing, it may make
# sense to cache the value of the inverse so that when we need it again, it
# can be looked up in the cache rather than recomputed.
# 
# Below are two functions that are used to create a
# special object that stores a numeric matrix and caches its inverse.

# The first function, `makeCacheMatrix` creates a special "matrix", which is
# really a list containing a function to
# 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse matrix
# 4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setinv`
# function.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i      ## Return a matrix that is the inverse of 'x'
}
