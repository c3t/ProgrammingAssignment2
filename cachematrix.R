## Functions to calculate and cache the inverse of a matrix. Assume supplied
## matrix is always invertible.

## makeCacheMatrix takes a matrix as an argument and creates special "matrix"
## object that can cache its inverse. Returns list of functions to set, get,
## setinverse and getinverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL               ## Initialise inverse value to null
    set <- function(y) {
        x <<- y             ## Change matrix in main function environment
        i <<- NULL          ## Initialise inverse due to matrix change
    }
    get <- function() x     ## Return matrix x from main function
    
    ## Store/cache inverse in main function environment variable i
    setinverse <- function(inverse) i <<- inverse
    
    ## Return cached inverse from main function environment
    getinverse <- function() i
    
    ## Return list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes inverse of special "matrix" returned by makeCacheMatrix
## returns cached data if inverse is already calculated (& matrix not changed)

cacheSolve <- function(x, ...) {
    ## Assign cached matrix inverse (or NULL value) to i
    i <- x$getinverse()
    
    ## Test for NULL inverse value
    if(!is.null(i)) {
        ## Print message and return non-NULL cached matrix inverse
        message("getting cached data")
        return(i)                   
    }
    
    ## If cache is NULL, calculate, set and return inverse for matrix x
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
