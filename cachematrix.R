
# This algorithm calculates the inverse of a matrix. It stores the values of inverted matrix in _
# an external location, to use them in case we ask to compute the same inverse it already did calculate.

# Here the first part, an external location for the inverse of the matrix is created 
makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverso <- function(inverso) inv <<- inverso
    getinverso <- function() inv
    list(set = set, get = get,
         setinverso = setinverso,
         getinverso = getinverso)
  
}
#Now the part of the program that actually compute the inverse and look if it has
#already been calculated before:
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverso()
  if(!is.null(inv)) {
    message("getting cached data!")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverso(inv)
  return(inv)
}

