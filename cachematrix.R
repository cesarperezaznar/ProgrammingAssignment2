## "matrix"   objec to cache inverse

makeCacheMatrix <- function(x = matrix()) {
    # inverse store
    inv <- NULL

    # altering matrix invalidate cache 
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    # get will be used to get matrix data
    get <- function() {
        x
    }

    # setinv generates inv
    setinv <- function(i) {
        inv <<- i
    }

    # getinv generates inverse
    getinv <- function() {
        inv
    }

    # return matrix
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)    
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # get the cached inverse
    inv <- x$getinv()

    if(!is.null(inv)) {
        # if the inverse if actually cached, just return it
        message("getting cached inverse")
        return(inv)
    }

    # otherwise, calculate the inverse and cache it
    matr <- x$get()
    inv <- solve(matr, ...)
    x$setinv(inv)

    return(inv)
}


## Example:
# matr <- makeCacheMatrix(matrix(1:4, 2, 2))
# cacheSolve(matr)
# cacheSolve(matr)  ## "getting cached inverse"
# matr$set(matrix(5:8, 2, 2))
# cacheSolve(matr)
# cacheSolve(matr)  ## "getting cached inverse"
