# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions perform these actions.

# makeCacheMatrix: This function creates a special "matrix" object that can
# cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # initialize
    set <- function( y ){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function( inverse ) inv <<- inverse
    getinv <- function() inv
    list( set = set, get = get, setinv = setinv, getinv = getinv )
}


# cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve the
# inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
    inv <- x$getinv()
    if( !is.null(inv) ){ # check for the cached version
        message("Retrieving cached data...")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...) # use solve to get the inverse
    x$setmatrix(inv)
    inv # return
}
