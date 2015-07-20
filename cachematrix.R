## Below are two functions that are used to create a 
## special object that stores a matrix and it caches inverse. 

## The first function takes a matrix as input and assigns a variable for its 
## matrix inverse. The output is a list of four functions:
## 1.) get() : prints the matrix
## 2.) set(new_matrix) : changes the matrix to new_matrix
## 3.) setinv(new_matrix): changes the (supposed to be) inversed matrix to new_matrix
## 4.) getinv(): prints the stored (supposed to be) matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
          x <<- y
          xinv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) xinv <<- inv
    getinv <- function() xinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The follwing second function takes a matrix encoded in a list made with makeCacheMatrix
## and prints the cached (supposed to be) inverse of that matrix.
## If the inverse was not assigned yet, the function will first compute it and put 
## it in the input list before printing. 


cacheSolve <- function(x, ...) {
    xinv <- x$getinv()
    if(!is.null(xinv)) {
        message("getting cached data")
    } else {
    data <- x$get()
    xinv <- solve(data, ...)
    x$setinv(xinv) 
    }
    return(xinv)
}
