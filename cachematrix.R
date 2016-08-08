## Creates a special "vector"
## This is for the R Programming Course Week 3 Assignament
## This function will do:
##      Get/Ser the value of the vector
##      Get/Set the value of the mean


makeCacheMatrix <- function(x = matrix()) {
    myinverse <- NULL 
    set <- function(y) { 
        x <<- y 
        myinverse <<- NULL 
    } 
    get <- function() x 
    setinverse <- function(inverse) myinverse <<- inverse 
    getinverse <- function() myinverse 
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}


## Calculates the inverse of a matrix
## If inverse is cached then return values from cached data

cacheSolve <- function(x, ...) {
     myinverse <- x$getinverse() 
    if(!is.null(myinverse)) { 
        message("getting cached data.") 
        return(myinverse) 
    } 
    data <- x$get() 
    myinverse <- solve(data) 
    x$setinverse(myinverse) 
    myinverse
} 

