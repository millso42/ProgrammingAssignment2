## Week 3 Assignment 2
## Gary Mills (millso42)

## Put comments here that give an overall description of what your
## functions do:

#This cachemmatrix.R file contains two functions:
# makeCacheMatrix() and cachesolve(). 
# The first function in the file, makeCacheMatrix() 
# creates a matrix and its cached inverse. 
# The second function, cachesolve() 
# uses the argument from makeCacheMatrix() 
# in order to calculate using solve function or to get the inverse from the 
# cached value if the matrix did not change.

## Creates a matrix and once cacheSolve is run it caches its 
# inverse. This creates the set, get, setInverse, getInverse in order that 
# we can reference the piece of code with the $ e.g. x$get()

makeCacheMatrix <- function(x = matrix()) {     #creates a matrix to populate
        i <-NULL                                # using i as short for inverse 
                                                #(mean example used m for mean)
        set <- function(y) {
                x <<-y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) i <<- Inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Calculates the inverse of the matrix created from makeCachematrix 
# or if the result for the same matrix has already been calculated
# it returns the value from the cache

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)           #solve function to calculate inverse of
                                        # the  matrix
        x$setInverse(i)
        i
}

# testing the code with a matrix m1 to get its inverse, then feeding that 
# back in (n1)  to get its inverse (i.e. original matrix m1) 
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1

myMatrix_object_m1 <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object_m1)

# running this again should give the comment getting cached inverse and print 
# the inverse from cache
cacheSolve(myMatrix_object_m1)

n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
n1
myMatrix_object_n1 <- makeCacheMatrix(n1)
cacheSolve(myMatrix_object_n1)
# running this again should give the comment getting cached inverse and print 
# the inverse from cache
cacheSolve(myMatrix_object_n1)
