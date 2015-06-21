## The functions in this R file cache the inverse of a matrix.
## The purpose of caching the inverse of a matrix is to save 
## computation time when, for example, the inverse of a large 
## matrix is needed.

## This assignment assumes that the matrix provided will be 
## invertible.

## makeCacheMatrix generates a list that sets and gets the
## value of a matrix and then sets and gets the inverse of
## that matrix.

makeCacheMatrix <- function(a = matrix()) {
        m <- NULL
        setMatrix <- function(z) {
                a <<- z
                m <<- NULL
        }
        
        getMatrix <- function() a
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)

        }

## cacheSolve computes the inverse of a matrix.  When the 
## inverse has already been computed, cacheSolve will 
## retrieve the inverse from the cache.

cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- a$getInverse()
        if(!is.null(m)) {
                message("Retrieving cached data")
                return(m)
        }
        
        data <- a$get()
        m <- solve(data)
        a$setInverse(m)
        m
}