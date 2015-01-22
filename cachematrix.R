## Below two functions can used to cache inverse of a matrix. I
## have added an example of usage at the bottom.

## This function returns a cached matrix object which can be used 
## to get and set the value of the matrix as well as the inverse
## of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(myMatrix) {
        x <<- myMatrix
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(myInverse) inverse <<- myInverse
    getInverse <- function() inverse
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function solves for the inverse of a invertible matrix. It takes as 
## parameter a cached matrix object and returns inverse of the matrix. Inverse
## is calculated if it has not been calculated in the past and stored in the 
## object passed. If inverse has already been calculated once, the inverse value 
## is directly returned. The function assumes that the matrix is always invertible  
## as mentioned in the problem statement

## A Inv(A) = I, where I is the identity matrix. So in the function below, Identity
## matrix is calculated using diag(n), where n is the number of rows(or columns) in
## matrix to be inverted.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    numberOfRows <- nrow(data)
    inverse <- solve(data, diag(numberOfRows), ...)
    x$setInverse(inverse)
    inverse
}

myMatrix <- matrix(c(4,3,3,2), nrow=2, ncol=2)
cachedMatrixObject <- makeCacheMatrix(myMatrix)
cacheSolve(cachedMatrixObject)
cacheSolve(cachedMatrixObject)

