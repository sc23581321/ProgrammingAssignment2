## Assignment 2, Cache the inverse of a matrix


# generate inverse matrix of a input square matrix
makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    
    get <- function() x
    
    setim <- function(solve) im <<- solve
    
    getim <- function() im
    
    list(set = set, get = get,
         setim = setim,
         getim = getim)
}


## retrive a cached inverse matrix if it's been previously solved

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    im <- x$getim()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setim(im)
    im
}
