#######################################################
## Caching the inverse of a matrix 
## The following functions compute the inverse of 
## a matrix and cache it for subsequent use.  
#######################################################

## The following function 
### 1) sets the value of the matrix
### 2) gets the value of the matrix
### 3) sets the value of the inverse of the matrix
### 4) gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) m <<- inverse
    get_inverse <- function() m
    list(set = set, 
         get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

## The following function: 
### Checks if the inverse of a "matrix" has been calculated.
### If so, it gets the inverse from the cache. 
### otherwise, it calculates the inverse of the data and 
### sets the value of the inverse in the cache via the 
### the set_inverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$get_inverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    dat <- x$get()
    m <- solve(data, ...)
    x$set_inverse(m)
    m
}