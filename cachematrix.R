# This 'cachematrix.R' file consists of two functions, 'makeCachematrix' and 'cacheSolve', where:
# - 'makeCachematrix' sets up an interface for setting and getting a matrix and it's inverse
# - 'cacheSolve' checks if a previous calculated inverse matrix already exists. If so, it returns this, if not the inverse of the matrix is calculated for the first time and stored. For large matrices this caching process can save time and processing power.

# First here is the 'makeCacheMatrix' function. This is similar to the 'makeVector' example, except:
# - instead of setmean and getmean, I used setinverse and getinverse
# - in setinverse, solve is called, not mean
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Now for the 'cacheSolve' function. This is the same as the 'cachemean' example, except:
# - m has been replaced by s (again to represent 'solve')
# - getmean() has been replaced by getinverse()
# - 'mean' function has been replaced by 'solve'

cacheSolve <- function(x, ...) {
  m <- x$getInverse()   ## return inverse matrix of x
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
