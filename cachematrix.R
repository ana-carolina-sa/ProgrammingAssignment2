## Here we create two functions to study Lexical Scoping in R. One function ("makeCacheMatrix") 
## will cache the inverse of a matrix passed as argument and another function ("cacheSolve")
## will retrieve the inverse matrix 
#
#
#
#
## This function is initialized by defining an empty object to store the inverse matrix (m) latter in the code. 
## Next, "getters and setters" are defined to set and get, respectively, the data values of the matrix and its 
## inverse. Finally, the setters/getters functions are assigned to a list.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        set <- function(y){
                
                x <<- y
                m <- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) m <<- inverse
        
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse, getinverse = getinverse)
        
}
#
#
#
#
#
## This function retrieves the inverse of an object of type makeCacheMatrix. The following actions will take 
## place in the code: attempt to retrieve the inverse of the matrix (x) passed as argument; if the inverse is 
## not NULL and was alredy calculated, a message will be returned; otherwise, the inverse matrix will be 
## calculated. The inverse matrix will be printed. 


cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m        
}





















