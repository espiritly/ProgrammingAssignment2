## Functions allow user to take inverse of a matrix and put and retrieve inverse from cache

## Create matrix and put matirx and it's inverse into cache

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {  ## set value of matrix
        x <<- y
        s <<- NULL
    }
    get <- function() x ## get value of matrix
    setinverse <- function(solve) s <<- solve  ## set value of inverse
    getinverse <- function() s  ## get value of inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ##cache
}


## Pull matrix from cache and take inverse

cacheSolve <- function(x, ...) {
    s <- x$getinverse()  ## retrieving inverse from cache
    if(!is.null(s)) {  ## check that inverse has been calculated
        message("getting cached data")
        return(s)
    }
    data <- x$get()  ## retrieving matrix
    s <- solve(data, ...)  ## calculate inverse
    x$setinverse(s)  ## store inverse into cache
    s  ## Return a matrix that is the inverse of 'x'
}

