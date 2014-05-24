## Functions to compute and cache the inverse of a matrix

## Function to create an object representing the matrix and
## its inverse, in the form of a list of setters and getters.

makeCacheMatrix <- function(x = matrix()) {
        ## x is the matrix
        ## i is the inverse matrix
        i <- NULL

        ## Setter and getter for the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }

        get <- function() x

        ## Setter and getter for the inverse
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i

        ## List representation of the matrix and its inverse
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function to compute the inverse of a matrix representation,
## or return the cached value (if there is one).

cacheSolve <- function(x, ...) {
        ## Get the (cached) inverse of the matrix
        i <- x$getinverse()

        ## Check whether there is indeed a cached inverse
        if(!is.null(i)) {
                ## There is, so return it
                message("getting cached data")
                return(i)
        }

        ## There is no cached inverse, so get the matrix
        data <- x$get()

        ## Calculate the inverse of the matrix
        i <- solve(data, ...)

        ## Cache the inverse, and then return it
        x$setinverse(i)
        i
}
