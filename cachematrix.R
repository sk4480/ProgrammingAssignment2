## This program calculates inverse of square-matrix given as input.
## It makes up 2 main functions. makeCacheMatrix() function inverse and cache a given matrix.
## CacheSolve() calls getinvert() from the 1st function,If it reads it returns the ouput.
## If not, it calculates the inverse using solve() and sets the value using makeCacheMatrix$setinvert. 

## makeCacheMatrix() takes input as a matrix
## set() - sets value for x and m and superassign (<<-) it to parent environment.
## get() - read the matrix from parent environment (lexical scoping)
## setinvert() - value of m (inverse matrix) is assigned to parent environmet
## getinvert() - Reads the inverted matrix (m)
## makeCacheMatrix() does not check if a matrix is square, or if it is a singular/inversible (ie. determinant=0)

## Test Run
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
      list(set = set, get = get,
           setinvert = setinvert,
           getinvert = getinvert)
}

#cacheSOlve() takes input matrix 'x'. If getinvert() does not return NULL, it retruns already stored 'm'.
#Else, it uses solve() to calculate inversion,stores it in 'm', and returns 'm'. 
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

