##Functions that deal with inverting invertible matrices


##Creates list of functions that holds matrix x and its cached inverse  
makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    list(get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## Returns inverse matrix of x
cacheSolve <- function(x, ...) {
    
    inv <- x$getinv()
    
    ## Test if inverse matrix already cached
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    ## Gets inverse using solve() if not cached yet
    inv <- solve(x$get(), ...)
    
    ##Puts inverse in cache
    x$setinv(inv)
    
    inv
}
