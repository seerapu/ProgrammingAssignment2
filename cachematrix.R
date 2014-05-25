# Following functions creates a special matrix that can cache its inverse. 
# But if the inverse already exists then it returns the value from the cache.
# 
# Usage:
# > x <- matrix(rnorm(4), nrow = 2)         
# > zx <- makeCacheMatrix(x)            
# > zx$get()                     
# > cacheSolve(zx)                       
# > cacheSolve(zx)   // Second time to return the cached inverse                      
                                          


# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setinvm <- function(inverse) invm <<- inverse
        getinvm <- function() invm
        list(set = set, get = get,
             setinvm = setinvm,
             getinvm = getinvm)        
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        invm <- x$getinvm()
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        data <- x$get()
        invm <- solve(data, ...)
        x$setinvm(invm)
        invm
}

