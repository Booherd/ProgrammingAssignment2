## These two functions take a matrix as an argument, then create a chache value containing the inverse of that matrix

## The first function takes the matrix and initializes the variables in the parent environment that will be needed to determine if the inverse has already been calculated

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
set <- function(y) {
        x <<- y
        m <<- NULL
}
get <- function() x
setinverse <- function(solve) m <<- solve
getinverse <- function() m
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}

## this function takes as its argument the "special vector" created by the previous function and uses it to determin if the inverse of the matrix has been calculated yet.  If the value has not yet been calculated, it runs the solve() function and returns the inverse.  If the value has already been calculated, it simply returns that stored value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
