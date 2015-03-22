## These functions allow for the cacheing of the inverse (solution) to a matrix, so 
## subesquent solves can be performed much more quickly. These funtions create a special
## object that stores a matrix and caches it's inverse.

## This function creats a list that contains functions to set and get a matrix and get and set
## the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # m will store the inverse of the matrix. It is NULL if the inverse
                  # has not yet been cached
        set <- function(y){ #defines the values/size of the matrix
                x <<- y 
                m <<- NULL
        }
        get <- function() x # returns the matrix
        setsolve <- function(solve) m <<- solve # solves and caches the solution
        getsolve <- function() m # retrieves the solution from the cache
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


## This function will get the cached solution to the matrix if it has been previously solved,
## or solve and chache the solution if it hasn't been.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve() # sets m to the solution if the matrix has been solved previously
        if(!is.null(m)) {
                message("getting cached data") # Returns the chached solution
                return(m)
        }
        data <- x$get() 
        m <- solve(data, ...) # Solves the matrix if it hasn't been solved before
        x$setsolve(m) # Caches the solution.
        m
}
