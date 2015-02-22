## Functions makeCacheMatrix and cacheSolve return an inverse matrix.

## makeCacheMatrix vreates a list of four functions:
##1. set the value of the vector
##2. get the value of the vector
##3. set the value of the mean
##4. get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
               inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(a) inv <<- a 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## cacheSolve first checks if inside variable inv of makeCacheMatrix function is not null. If so,
##function returns the inverse matrix from the cache, otherwise it calculates the inverse of the data
##and sets the value of a in the cache via the solve function.

cacheSolve <- function(x, ...) {
        inv = x$getinv()

        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        mat.data = x$get()
        inv = solve(mat.data, ...)

        x$setinv(inv)
        
        return(inv)
}
