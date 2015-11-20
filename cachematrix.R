# Coursera R Programming Course project
#
# Caching the inverse of a matrix
#

## Creates a matrix that can cache it's inverse
# It has 4 inbuilt functions: set/get matrix and set/get inverse of the matrix
makeCacheMatrix <- function(x = matrix()){
    # set the value of the matrix
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    # get the value of the matrix
    get <- function() x
    # set the value of the inverse
    setinverse <- function(inverse) m <<- inverse
    # get the value of the inverse
    getinverse <- function() m
    # list the result set
    list(set=set,get=get, setinverse=setinverse,getinverse=getinverse)
}

## Either calculate the inverse of a matrix and cache it, or retrieve the cached inverse

cacheSolve <- function(x,...){
    # Return a matrix that is the inverse of 'x'  
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cahced data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}