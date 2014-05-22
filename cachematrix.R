## makeCacheMatrix function will set and get the inverse of the matrix 
## fuctions need to be initiated to work 

## macheCacheMatrix has set and get. set will store matrix to variable and get will
##retrieve from cache variable or calculate using function below

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## either calculates the inverse of the matrix that was set or 
## gets the inverse of the matrix previously calculated. 

cacheSolve <- function(x, ...) {
    m <- x$getinverse() 
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## returns a matrix that is the inverse of 'x'
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
