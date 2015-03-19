## Below are two functions required for the Coursera R programming assignment 2
## The purpose of this assignment is to write functions that cache the inverse of a matrix
## We assume that the matrix used in these functions will always be invertible

## makeCacheMatrix will create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    #set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <- NULL
    }
    # get the value of the matrix
    get <- function() x
    
    # set the value of the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
    
    # get the value of the inverse of the matrix
    getInverse <- function() inv
    list(set = set, get= get, setInverse = setInverse, getInverse = getInverse)

}


## cacheSolve will compute the inverse of the special "matrix" object returned
## by by makeCacheMatrix. If the inverse has already been calculated, then the
## this function should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve (data)     # the solve function computes the inverse of a square matrix
        x$setInverse(inv)
}

# Below is an output of a test run I did with the above two functions 

  ## create a random (invertible) matrix
       # test_matrix = rbind(c(10, -1/2), c(-1/2, 10))
  ## test the function using this matrix
      # test_function = makeCacheMatrix(test_matrix)
      # test_function$get()
          # [,1] [,2]
          # [1,] 10.0 -0.5
          # [2,] -0.5 10.0

  ## run the cacheSolve function
      # cacheSolve(test_function)
          # nothing to return seeing that there is no cache

          # running it again will return the inverse of the matrix
          # due to the fact that there is now something in the cache
          # that the function can retrieve
     
      # cacheSolve(test_function)
          # getting cached data
          # [,1]        [,2]
          # [1,] 0.100250627 0.005012531
          # [2,] 0.005012531 0.100250627