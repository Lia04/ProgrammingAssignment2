## These functions create a special object "matrix" that can cache it's
## inverse and compute the inverse of matrix, if it's not stored in cache

## This function (makeCacheMatrix) can set and get the matrix and also can get
## or set the inverse matrix. It create a special object "matrix" that can cache
## it's inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ##get or set the value for matrix x
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        ## get or set the inverted matrix
        setinv <- function(invers) inv <<- invers
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function (cacheSolve) calculates the inverted matrix for x(if it doesn't
## exist) or retrieve the inverse from cache.It returns the inverted matrix inv

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        ##Checking the existing of inverted matrix in cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ##Calculating the inverted matrix for x
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
