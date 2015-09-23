## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly. The 
## functions written below help to cache the inverse of a matrix.

## The function makeCacheMatrix contains a list of functions that:-
## 1.Set the value of the matrix
## 2.Get the value of the matrix
## 3.Set the value of the inverse of the matrix
## 4.Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates the inverse of the matrix created above.
## If the inverse has already been calculated, it gets the inverse from the 
## cache and skips the computation. Else it calculates the inverse and sets
## the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) 
{
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data!")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
