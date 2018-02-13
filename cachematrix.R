## Put comments here that give an overall description of what your
##Our aim in this experiment is to write a pair of functions, namely, 
## functions do
##"makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix.

## Write a short comment describing this function
##makeCacheMatrix is a function which creates a special "matrix" object that can 
## cache its inverse for the input (which is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
 +      inv <- NULL
 +  set <- function(y) {
 +    x <<- y
 +    inv <<- NULL
 +  }
 +  get <- function() x
 +  setinv <- function(inverse) inv <<- inverse
 +  getinv <- function() inv
 +  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function
## cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        +  inv <- x$getinv()
 +  if(!is.null(inv)) {
 +    message("getting cached result")
 +    return(inv)
 +  }
 +  data <- x$get()
 +  inv <- solve(data, ...)
 +  x$setinv(inv)
 +  inv
}
+
 +## ---------------Checking the program------------------------
 +## m <- matrix(rnorm(16),4,4)
 +## m1 <- makeCacheMatrix(m)
 +## cacheSolve(m1)
  +##[,1]       [,2]       [,3]       [,4]
+##[1,]  0.4192607  5.3274321 -4.0264164 -0.9581632
+##[2,] -0.5765262  0.6614953  0.1872941 -0.5119873
+##[3,]  1.4111501  7.8159995 -5.4557238 -1.2051524
+##[4,] -0.9821686 -6.6254348  5.2877348  2.0199097
+ 
