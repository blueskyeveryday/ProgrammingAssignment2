## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- matrix()
    
    set <- function(y) {
      x <<- y   
      m <<- matrix() 
    }
    get <- function() x
    setinver <- function(solve) m <<- solve
    getinver <- function() m
    list(set = set, get = get,
         setinver = setinver,
         getinver = getinver)
  
  
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.Write a short comment 
## describing this function

cacheSolve <- function(x, ...) {
    m <- x$getinver()
    print(m)
    if(!is.na(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinver(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
