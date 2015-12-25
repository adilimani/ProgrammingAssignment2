## The code below includes two functions, whose purpose is to cache the inverse of a Matrix
## If Inverse Matrix exists in cache, then the program will utilize that rather than re-calculating
## This should cut down on computationally intensive calculations when not required


## The makeCacheMatrix function sets the value of matrix, gets the value of matrix,
## sets a value for the inverse matrix for it, and gets the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    InvM <- matrix()
    set <- function(y) {
        x <<- y ##Defining x in the main (makeCacheMatrix) function
        InvM <<- matrix() ##Defining InvM in the main (makeCacheMatrix) function
    }
    get <- function() x
    setinverse <- function(inverse) InvM <<- inverse
    getinverse <- function() InvM
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function checks if inverse has been calculated (and stored in cache)
## Only if it does not have an inverse then it calculates it, and saves as matrix.
## FInally, it saves down the new value of the inverse to th cache for future use. 

cacheSolve <- function(x, ...) {
    InvM <- x$setinverse()
    if(!is.null(InvM)) { ##Checks if matrix exists and is not null
        message("getting cached data")
        return(InvM) #Returning from cache if it exists
    }
    data <- x$get()
    InvM <- Solve(data, ...)%*%data ##Calculation of inverse, assumes an invertible matrix
    x$setinverse(InvM) ##Saving to cache
    InvM ## Return a matrix that is the inverse of 'x'
}
