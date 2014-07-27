## Put comments here that give an overall description of what your
## functions do

## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly
## cachematrix.R creates a special "matrix" object that can cache its inverse.


# makeVector creates a special "vector", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix


## Write a short comment describing this function



makeCacheMatrix <- function(x = matrix()) {
        # 'matrixInverse' variable will store the cached inverse of the matrix
        # Intitialize 'matrixInverse' is null
        
        matrixInverse <- NULL
        
        # 'set' function used to set the value of input matrix
        set <- function(y){
                x <<- y
                matrixInverse <<- NULL
        }
        
        # 'get' function used to get the value of input matrix
        get <- function() x
        
        # 'setinverse' function used to cache the value of inverse
        setinverse <- function(solve) matrixInverse <<- solve
        
        # 'getinverse' function used to get the cached value of inverse
        getinverse <- function() matrixInverse
        list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## Write a short comment describing this function

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Get cached value of the inverse
        s <- x$getinverse()
        
        # If s is not NULL, then the input matrix has not changed and the s contains
        # the cached value of the matrix, the return s
        if (!is.null(s)) {
                message ("getting cached data")
                return (matrixInverse)
        }
        
        # If s is NULL, get the new value of the input matrix for calculating inverse
        data <- x$get()
        
        # Solve the matrix for inverse and save the value of inverse in s
        s <- solve(data)
        
        # Cache the new value of inverse 
        x$setinverse(s)
        
        # Return the updated value of inverse
        s
}
