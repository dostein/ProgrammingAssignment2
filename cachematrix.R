##  makeCacheMatrix and cacheSolve are being defined
## They  are used to create a special object that stores a matrix and cache's its inverse


# makeCacheMatrix creates a  "vector", which is  a list containing a function to
# 1) set the value of the matrix
# 2) get the value of the matrix
# 3) set the value of the inversed matrix
# 4) get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        set.inverse <- function(solve) m <<- solve
        get.inverse <- function() m
        list(set = set , get = get ,
             set.inverse = set.inverse,
             get.inverse = get.inverse)
        
        

}



# cacheSolve calculates the inverse of the  "matrix" created with the makeCacheMatrix function. 
# It first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation.
# Else, it calculates the inverse of the matrix and sets the value of the inverse ( using "solve" ) 
# in the cache via the set.inverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$get.inverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set.inverse(m)
        m
        
}
