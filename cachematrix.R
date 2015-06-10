## This pair of functions can be used to make special matrix objects
## that store their inverse so it does not need to be computer more
## than once.
## makeCacheMatrix - makes such objects
## cacheSolve - solves for the inverse like solve, but uses cache is exists

## This function takes a matrix and creates an object
## that stores that matrix (x) and inverse (inv), and
## associated functions to get and set x and inv

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) {inv <<- inverse}
    getinv <- function() {inv}
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function takes an object of the type above and returns the inv
## Structure: 1) checks if a stored inv exists, if so returns it
## 2) otherwise it finds the inv and retures it

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
