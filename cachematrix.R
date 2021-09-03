## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix sets and creates a matrix as well as the inverse 
# and caches the inverse. 
#cacheSolve checks if the inverse of the matrix was already made
# by the previous function and either retrieves the cache matrix
# inverse, or uses solve() to calculate the inverse.

makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        get <- function() x
        setmatinv <- function(inverse) matinv <<- inverse
        getmatinv <-function() matinv
        list(set = set, get = get,
             setmatinv = setmatinv,
             getmatinv = getmatinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matinv <- x$getmatinv()
        if (!is.null(matinv)) {
                message("getting cached data")
                return(matinv)
        }
        data <- x$get()
        matinv <- solve(data, ...)
        x$setmatinv(matinv)
        matinv
}
