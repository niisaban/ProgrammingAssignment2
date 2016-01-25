## https://github.com/niisaban/ProgrammingAssignment2.git
## 1st commit SHA-1 hash identifier: 6adfd9f580a99dba9587ab6a54ff7f61386d74e8
## R Programming Assignment 2: Lexical Scoping
## This assignment "Caches the Inverse of a Matrix"
## functions do

## the makeCacheMatrix() function creates a special "matrix" object that can cache its inverse,
## There are two functions. The first function is a list to
## 1. set the value of the matrix
## 2. get the value of the vector
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        z <- NULL
        set <- function(y) {
                x <<- y
                z <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) z <<- inverse
        getinverse <- function() z
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The cacheSolve() function calculates the inverse of the special 'matrix' created with the function above
## It however, first checks to see if the inverse has already been calculated
## If so, it gets the inverse from the cache and skips the computation
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        z <- x$getinverse()
        if(!is.null(z)) {
                message("getting cached data")
                return(z)
        }
        data <- x$get()
        z <- solve(data, ...)
        x$setsolve(z)
        z
}
