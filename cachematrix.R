## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


# creates a special "matrix", which is really a list containing functions to
#    set the value of the matrix
#    get the value of the matrix
#    get the cached value of the inverse of the matrix (getCachedSolve)
# Example:
#   x<-makeCacheMatrix(rbind(c(1,2), c(1,1))
#   x$get()
#       [,1] [,2]
#[1,]    1    2
#[2,]    1    1
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL # 'inverse' is cached value of inverted matrix

        #define a function to set new value of matrix
        set <- function(y) {
                x <<- y          # set x value to new matrix y
                inverse <<- NULL # and clears cached value of inverted matrix
        }

        #define a function to get value of matrix
        get <- function() x

        #define a function to get cached value of inverse of the matrix
        getCachedSolve <- function(...) {
                if(!is.null(inverse)) {
                        # if cached value exists, just return it
                        message("getting cached data")
                        return(inverse)
                }
                # otherwise calculate inverse of the matrix
                inverse <<- solve(x, ...)
                # return value of inverse of the matrix
                inverse
        }

        # return a list of those 3 functions
        list(set = set, get = get, getCachedSolve = getCachedSolve)
}

## Return a matrix that is the inverse of 'x'
# x MUST be created with makeCacheMatrix function
# Example of how to test:
## 1) Create 'special' matrix using makeCacheMatrix
#
#> x <- makeCacheMatrix(rbind(c(1,2), c(1,0)))
#
## 2) Check that returned matrices via cacheSolve(x) and solve(x$get())
##    are the same
#
#> identical(cacheSolve(x), solve(x$get()))
# [1] TRUE  #Output MUST be TRUE for any invertible matrix
#
## 3) Check that next call of cacheSolve returns caches value, e.g.
##    cacheSolve(x) will produce "getting cached data" message in the output
#
#> identical(cacheSolve(x), solve(x$get()))
# getting cached data
# [1] TRUE
#
## 4) Check that 'set' function clears cached inverse of the matrix, e.g.
#     there is no message 'getting cached data'  in the output
#
#> x$set(rbind(c(2,3),c(3,4)))
#> identical(cacheSolve(x), solve(x$get()))
#[1] TRUE
#
cacheSolve <- function(x, ...) {
        # just call 'getCachedSolve' method of x
        x$getCachedSolve(...)
}
