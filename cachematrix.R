## This library defines a 'special' matrix object which stores its inverse matrix
## to avoid redundant operations

## This function creates a 'special' matrix which allows to cache its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    #Gives the matrix value
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
   
    #Gives the matrix value
    get <- function() x  
    #Sets the matrix inverse
    setSolve <- function(solve) inv <<- solve
    #Gives the inverse matrix value
    getSolve <- function() inv
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This function returns the inverse matrix, if it is not cached it calculates the
## inverse and stores it in the 'special' matrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getSolve()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }else{
        message("not cached data, calculating inverse...")    
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setSolve(inverse)
    inverse
}
