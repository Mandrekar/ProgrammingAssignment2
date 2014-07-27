
## The function below creates a special  matrix that is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(matrix) {
                m <<- matrix
                inv <<- NULL
        }
        get <- function() m
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse,getInverse = getInverse)
}


## The function below computes the inverse of the matrix which is created by makeCacheMatrix function
#and if the inverse is calculated apriori, the function below will fetch the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        # Compute the inverse of the matrix using matrix multiplication
        m <- solve(data)
        x$setInverse(m)
        m
}
