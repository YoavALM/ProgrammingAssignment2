## Functions below allow caching the inverse of a matrix rather than compute it repeatedly as - long
## as it hasn't changed. 

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  Inv <- NULL
  set <- function(New_x) {
                            x <<- New_x
                            Inv <<- NULL
                         }
  get <- function() x
  setinv <- function(New_Inv) Inv <<- New_Inv
  getinv <- function() Inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## his function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  Inv <- x$getinv()
  if(!is.null(Inv)) {
                      message("getting cached data")
                      return(Inv)
                    }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setinv(Inv)
  Inv
  
}
