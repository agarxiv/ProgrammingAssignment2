#############################################
#
# title: Solution to rprog-009, Project 2
# author: argarxiv
# date: November 2014
#
# Matrix inversion is usually a costly computation and their may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly (there are
# also alternatives to matrix inversion that we will not discuss here). 
# Below a pair of functions have been written that cache the inverse of a matrix.
# These functions are:
# 1. "makeCacheMatrix": This function creates a special "matrix" object that can 
#    cache its inverse.
# 2. "cacheSolve": This function computes the inverse of the special "matrix" 
#    returned by makeCacheMatrix above. If the inverse has already been calculated 
#    (and the matrix has not changed), then the "cachesolve" retrieves the inverse 
#    from the cache.
#
# NB. Computing the inverse of a square matrix has been done with the solve function 
# in R. For example, if X is a square invertible matrix, then solve(X) returns its 
# inverse.
#
# For this assignment, we assume that the matrix supplied is always invertible.
#
#############################################


# The "makeCacheMatrix" function creates a special "matrix" object that can cache its inverse, by:
# 1. setting the value of the matrix
# 2. getting the value of the matrix
# 3. setting the value of the inverse of the matrix, using R's solve() function 
# 4. getting the value of the inverse of the matrix
# 5. The values for 1--4 are then name and made accessible as a list
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
		setInverse <- function(solve) m <<- solve
		getInverse <- function() m
        list(set = set, get = get,
			setInverse = setInverse,
			getInverse = getInverse)
}


# The "cacheSolve" function computes the inverse of the special "matrix" returned by the 
# "makeCacheMatrix" function above, by:
# 1. Calculating the inverse of the special "matrix" created with the above function.
# 2. First, it checks to see if the inverse matrix has already been calculated.
# 3. If so, it gets the inverse from the cache and skips the computation.
# 4. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse 
#    in the cache via the setInverse function.
# 5. If the inverse has already been calculated (and the matrix has not changed), then the 
#    "cacheSolve" function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
		m <- x$getInverse()
		if(!is.null(m)) {
			message("getting cached data")
            return(m)
		}
		mat <- x$get()
		m <- solve(mat, ...)
		x$setInverse(m)
		m
}
