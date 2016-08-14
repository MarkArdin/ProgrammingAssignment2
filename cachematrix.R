
# Function specification.
# Matrix inversion is usually a costly computation and this function provides the ability to cache 
# the results for the inverse of a matrix. 
#
# The function expects the provided matrix to be invertible.
# The makeCahceMatrix function will return a list of the following 4 functions

# Function set - sets the value of the matrix
# Function get - gets the value of the matrix
# Function set - inverts and sets the value of the matrix
# Function get - returns the value of the inverted matrix

# The cacheSolve function inverts the matrix and caches the value
# Examples and instructions of how to use are included at the end of the file


makeCacheMatrix <- function(x = matrix()) {

        # Initialise a variable to hold the inverted matrix
        inv <- NULL
        
        # Define the setmatrix function which sets x to the input value of y
        setmatrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Define the get matrix function which returns the value of x (the Matrix)
        getmatrix  <- function() x
        
        # Define the setinverse function which takes the inverted matrix and sores it into inv
        # This is designed to be called by cacheSOlve but can be called independently if needed
        setinverse <- function(inverse) inv <<- inverse
        
        # Define the function getinverse which returns the value of inv     (the inverted matrix)
        getinverse <- function() inv

        # Return the list of funntions
        list(set=setmatrix,
             get=getmatrix, 
             setinverse=setinverse, 
             getinverse=getinverse)
}

# The cacheSolve function returns the inverted value of a supplied matrix. 
# If the inverted matrix has already been cached it will return the cached version (the peformance gain)
# Otherwise it will invert the matrix and caches the value in inv using the setinverse function

cacheSolve <- function(x, ...) {
        
        # Obtain the value of inv (inveretd cache value)
        inv <- x$getinverse()
        
        # Check the value of inv to see if it is already cached - if so return the cached value
        if(!is.null(inv)) {
                print("Inverted Matrix 1 data being obtained from the cache")
                return(inv)
        }
        
        # If the inverted value has not been cached, obtain the matrix value, invert and cache it
        matrixdata <- x$get()
        invertedmatrixdata <- solve(matrixdata)
        x$setinverse(invertedmatrixdata)
        invertedmatrixdata
}

# This example shows how to use ontwo different Matrices
# > myData <- matrix (1:4, 2,2);
# > myMatrix <- makeCacheMatrix(myData)
# > myMatrix$get()
# [,1] [,2]
# [1,]    1    3
#[2,]    2    4
# > cacheSolve(myMatrix)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(myMatrix)
# [1] "Inverted Matrix 1 data being obtained from the cache"
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > myMatrix$getinverse()    
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#
# > myData1 <- matrix (5:8, 2,2);
# > myMatrix1 = makeCacheMatrix(myData1)
# > myMatrix1$get()
# [,1] [,2]
# [1,]    5    7
# [2,]    6    8
# > cacheSolve(myMatrix1)
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
# > cacheSolve(myMatrix1)
# [1] "Inverted Matrix 1 data being obtained from the cache"
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
> myMatrix1$getinverse()        
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
