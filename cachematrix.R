## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix: creates a list containing a function to
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix

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


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x = matrix(), ...) {
    
    inv <- x$getinv()
    if( !is.null(m) ){
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setmatrix(inv)
    inv # return
}
