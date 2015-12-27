## Functions that cache the inverse of a matrix
## 
## Usage example
## 
## > source('cachematrix.R')
## > m <- makeCacheMatrix(matrix(c(1, 2, 3, 4), c(2, 2)))
## > cacheSolve(m)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Create a special "matrix", which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix
## 4. get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## Compute the inverse of the special "matrix" returned by the function above
## If the inverse has already been calculated, then function retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    i
}
