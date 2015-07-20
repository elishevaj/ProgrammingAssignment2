## These functions create a special object to store a 
## matrix and cache its inverse 


## This function creates a special "matrix" as a list
## containing functions to get and set value of matrix 
## and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
                
        }
        get <- function() x
        setinv <- function(i) inv <<- i
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function returns cached inverse of special "matrix"
## or calculates inverse to return if it has not yet 
## been calculated 

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv

}
