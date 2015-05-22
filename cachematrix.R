## Matrix inversion is usually a costly computation and there may be some benefit to 
##caching the inverse of a matrix rather than computing it repeatedly (there are also 
##alternatives to matrix inversion that we will not discuss here). Your assignment is 
##to write a pair of functions that cache the inverse of a matrix.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its 
##inverse.

##set function will set the value of matrix
##get function will get the value of matrix
##setinverse function will set inverse of matrix
##getinverse function will get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the matrix has
##not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is thex inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## In order ro run it please follow following steps
## > z<-matrix(1:4,2,2)
## > m<-makeCacheMatrix(z)
## > m
## Output would be
##$set
##function (y) 
##{
##        x <<- y
##        m <<- NULL
##}
##<environment: 0x10bd02378>
##        
##        $get
##function () 
##        x
##<environment: 0x10bd02378>
##        
##        $setinverse
##function (mean) 
##        m <<- solve
##<environment: 0x10bd02378>
##        
##        $getinverse
##function () 
##        m
##<environment: 0x10bd02378>
##cacheSolve(m)
