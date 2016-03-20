## This file contains two functions to easily handle the calculation for a Matrix inversion.

## makeCacheMatrix is a function that stores a list of four functions as below:
## 1.set function is to reset the Matrix from your oringinal input
## 2.get fucntion is to get the newest Matrix you assigened to a variable
## 3.setinverse is to store the vaule of a inversed Matrix to a variable
## 4.getinverse is to return the value of a inversed Matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSove will first check whether the cache has already stored the value inversed Matrix.
## If yes, the function will return the existing result.
## if no, the function will compute a new result from the Matrix value created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- 1 / data
        x$setinverse(i)
        i
}

## Run the code above to create these two function then run the following code to see an example.

a <- makeCacheMatrix(matrix(1:15, 3, 5))
a$get()
cacheSolve(a)
