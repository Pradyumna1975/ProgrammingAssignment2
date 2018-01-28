## Makecachematrix creates a matrix which will set and get matrix
## Get, Set values & Inverse

makeCacheMatrix <- function(x = matrix()) {

          inv <- NULL  
        set <- function(y) {
                x <<- y    
                inv <<- NULL  }  
        get <- function() x  
        setInverse <- function(inverse) inv <<- inverse  
        getInverse <- function() inv  
        list(set = set,       
             get = get,       
             setInverse = setInverse,       
             getInverse = getInverse)
                  
}


## Function computes the inverse of matrix. If the inverse is already calculated then
##  cachesolve should retrive the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 inv <- x$getInverse() 
        if (!is.null(inv))
        {    message("getting cached data")    return(inv)  } 
        mat <- x$get()  
        inv <- solve(mat, ...) 
        x$setInverse(inv) 
        inv
}
