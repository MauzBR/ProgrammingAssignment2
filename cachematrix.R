## Caching the inverse of a matrix

## The function make the a special matrix to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve(x)
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)  
}


## The function first check if there is any cache inverse. 
## If there is one, it simply return it from the original matrix
## If there is not, it calculate it using the

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- inverse(data)
        x$setinverse(inv)
        inv
}
