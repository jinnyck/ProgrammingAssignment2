## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix is a function that creates a special "matrix", 
# which can cache its inverse for the input.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse)m <<- inverse
    getinv <- function() m
    list(set = set, get = get, 
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

# cacheSolve is a function that calculates the inverse of the special "matrix"
# that created with the function above. If the inverse has already calculated,
# it will skip the computation and the cacheSolve function should retrieve the 
# the inverse from the cache. Otherwise, it calculates the inverse of the data 
# and sets the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- inverse(data, ...)
    x$setinv(m)
    m
}