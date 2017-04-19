## This program calculates inverse of a given square-matrix.
## It is made up of 2 main functions. makeCacheMatrix() function sets/reads matrix
## CacheSolve() calls getinvert() from the 1st function,If it finds it returns the ouput.
## Else, it computes the inverse using solve() and sets the value using setinvert(). 

## makeCacheMatrix() takes input as a matrix. The subfunction/output are:
## set() - sets value for x and m and superassign (<<-) it to parent environment.
## get() - read the matrix from parent environment (lexical scoping)
## setinvert() - value of m (inverse matrix) is assigned to parent environmet
## getinvert() - Reads the inverted matrix (m)
## It doesn't check if a matrix is square, or if it is a singular/not inversible. 

## Test Run
##---------
##> m1 <- makeCacheMatrix(matrix(rnorm(9),3,3))
##> cacheSolve(m1)
##Cache Miss - Inverting Matrix
##[,1]       [,2]       [,3]
##[1,] -0.2649747  -2.366984 -0.2644305
##[2,] -0.4680961  -1.531541 -0.6731020
##[3,] -4.8168199 -14.096261 -1.1283499
##> cacheSolve(m1)
##Cache Hit - Reading from the Cache
##[,1]       [,2]       [,3]
##[1,] -0.2649747  -2.366984 -0.2644305
##[2,] -0.4680961  -1.531541 -0.6731020
##[3,] -4.8168199 -14.096261 -1.1283499

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinvert <- function(invert) m <<- invert
      getinvert <- function() m
      #The list of output produced is named
      list(set = set, get = get,
           setinvert = setinvert,
           getinvert = getinvert)
     
}

#cacheSolve() calculates inverse of matrix. Output of makeCacheMatrix() is its input. 
#If getinvert() does not show NULL, it retruns already stored inverse from 'm'.
#Else, it uses solve() to calculate inverse of matrix,stores it in 'm', and returns 'm'. 
cacheSolve <- function(x, ...) {
      m <- x$getinvert()
      if(!is.null(m)) {
            cat("Cache Hit - Reading from the Cache\n")
            return(m)
      }
      message("Cache Miss - Inverting Matrix")
      data <- x$get()
      #Inverse matrix 'x' and store in 'm'.
      m <- solve(data, ...)
      x$setinvert(m)
      m
}

