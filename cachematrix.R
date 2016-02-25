## Put comments here that give an overall description of what your
## functions do
## This function finds inverse of a given matrix. 
##
## Write a short comment describing this function
## Returns inverse matrix
## Set inverse matrix, get inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## First it checks if inverse is available from earlier computation.
## If it finds then it return value from cache. Otherwise it calculate inverse.
## Return inverse of input matrix using makeCacheMatrix function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setmean(inv)
}
