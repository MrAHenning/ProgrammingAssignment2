# Student Name: A. Henning
# Date: 26 April 2020
# Coursera Course: R Programming

## Put comments here that give an overall description of what your
## functions do

## OVERALL DESCRIPTION:
## ------------------------------------
## To test this program, try the following:
##
##                  cacheSolve(makeCacheMatrix(matrix(c(2,3,4,1,-2,3,1,1,1),3,3)))
##
## Which should return an output of:
##
##                        [,1]  [,2]   [,3]
##                [1,] -0.625  0.25  0.375
##                [2,]  0.125 -0.25  0.125
##                [3,]  2.125 -0.25 -0.875
##
## In all honesty, this program is embarassingly similar to the example given in the program assignment, where
## you have to find and cache the mean of a vector. Really, the only difference here is that now we're exchanging
## the "mean" function for the "solve" function in "cacheSolve" [line 65 below], and writing a new function 
## "set_inverse" [line 39], where "setmean" is in the example. So, generally speaking, what happens here is: one
## function "makeCacheMatrix" creates a "special" matrix object that stores the matrix and caches its inverse.
##  --- For more information, please see the specific function descriptions below ----

## DESCRIPTION FOR "makeCacheMatrix"
## ------------------------------------
## Make Cache Matrix is a function that accomplishes the following:
##        1.) sets the value of the matrix
##        2.) gets the value of the matrix
##        3.) sets the value of the inverse, using the special <<- operator
##        4.) gets the value of the inverse
## The final result returns a list

makeCacheMatrix <- function(x = matrix()) {
      # Sets the value of inverse to NULL
      val <- NULL
      set <- function(y) {
            x <<- y
            val <<- NULL
      }
      get <- function() x
      set_inverse <- function(inverse){
            val <<- inverse
      } 
      get_inverse <- function() val
      list(set = set,
           get = get,
           set_inverse = set_inverse,
           get_inverse = get_inverse)
}


## DESCRIPTION FOR "cacheSolve"
## ------------------------------------
## This function calculates the inverse of the special matrix created with the "makeCacheMatrix" function. To do this,
## first it checks  if the inverse has previously been calculated. If so, it gets that inverse from the cache and 
## skips that calculation. If the inverse has not previouly been calculated, it calculates the inverse using "solve"
## and sets the inverse using the setInverse function.


cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      val <- x$get_inverse()
      if(!is.null(val)){
            message("Getting cached data")
            return(value)
      }
      mat <- x$get()
      val <- solve(mat, ...)
      x$set_inverse(val)
      val
}
