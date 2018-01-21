## This is work done by roiseal for R programming class
## functions included save effort of inversion of the matrix if one was already performed## and stored in cache for future reuse. 
## function mackeCacheMatrix does Matrix (square) inversion Cache 

makeCacheMatrix <- function(x = matrix()) {
             m <- NULL        
               set <- function(y) {                
                       x <<- y                
                       m <<- NULL        
               }        
        get <- function() x        
        setsolve <- function(solve) m <<- solve        
        getsolve <- function() m        
        list(set = set, get = get,             
              setsolve = setsolve,             
              getsolve = getsolve)
}

## cacheSolve returns inverse of the matrix doing inverse only if one is not already available

cacheSolve <- function(x, ...) {        
        ## Return a matrix that is the inverse of 'x'          
        
        m <- x$getsolve()         if(!is.null(m)) {                 
                message("getting cached data")                 
                return(m)         
        }         
        data <- x$get()         
        m <- solve(data, ...)         
        x$setsolve(m)        
        m
}
