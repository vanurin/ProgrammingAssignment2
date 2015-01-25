## Functions below provides functionality to find inverse matrix with caching.
## Storing inverse matrix in cache can save time if reated inversing of origina
## matirx is needed. 
##
## makeCacheMatrix --- is a function to store a matrix with its inverse matrix
## 	for caching. 
##
## cacheSolve --- computes the inverse of the special "matrix" returned by
##	 makeCacheMatrix. If the inverse has already been calculated (and the
##	 matrix has not changed), then cacheSolve should retrieve the inverse
##	 from the cache.


## This function creates a special "matrix" object that can cache its inverse.
## Returns a list of 4 functions:
## 1. to get original matrix,
## 2. to set original matrix,
## 3. to get inverse matrix,
## 4. to get inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse 
        getinv <- function() inv 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

