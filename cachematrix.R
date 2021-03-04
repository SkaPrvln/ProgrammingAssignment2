## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse, as it is written in the
##provided description. Simply it takes a matrix and inverts it, as it is provided in part "sample"
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the
###inverse from the cache, which is also shown in part "sample" .

## Write a short comment describing this function
##All the necessary description will be provided near the code for the better understanding.

## A function that creates a matrix capable of caching its inversion
makeCacheMatrix <- function(x = matrix()) { #just setting the matrix
  m <- NULL # setting the parameter of matrix invertibility
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x # getting our matrix
  setinverse <- function(solve) m <<- solve # just a method to get our matrix inverted
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) # computing the inverse with the help of the solve function
  x$setinverse(m)
  m # tracing back our matrix (return it)
}

#Sample

# we can see, how our function works on a simple example
a <- diag(6,6) #create a matrix (but it should be convertible)

a

CachedMarix <- makeCacheMatrix(a) #takes matrix and inverses it

cacheSolve(CachedMarix)##computes the inverse of the special "matrix" returned by makeCacheMatrix
