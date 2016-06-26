## The two functions below are used to create a new object that is a special matrix
# whose inverse is cached. 

##This function is a list of set and get functions for the matrix and for its inverse. 
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    myinv <- NULL
    set <- function(y) {
        x <<- y
        myinv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) myinv <<- inv
    getinv <- function() myinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}

##The following function calculates the inverse of the special "matrix"
#created with the above function. 

##It first checks to see if the mean has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse and sets the value of the inverse in the 
# cache via the setinverse function.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    myinv <- x$getinv()
    if(!is.null(myinv)) {
        message("getting cached data")
        return(myinv)
    }
    data <- x$get()
    myinv <- solve(data, ...)
    x$setinv(myinv)
    myinv
    
}
