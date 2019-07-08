## Creating a function that creates a special "matrix" object to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y){
            x <<- y
            inv <<- NULL
          }
          ## Method to get and return the matrix
          get <- function() x
          ## setting the inverse of matrix
          setInverse <- function(solveMatrix) inv <<- solveMatrix
          ## method to get the inverse of matrix
          getInverse <- function() inv
          list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Creating a function to calculate the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated  
## (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
          inv <- x$getInverse()
          ## return the inverse if its already cached
          if(!is.null(inv)){
            message("getting cached data")
            return(inv)
          }
    ## get the matrix from the object      
    data <- x$get()
    ## calculate the inverse
    inv <- solve(data)
    ## set the inverse to the object
    x$setInverse(inv)
    ## return the matrix
    inv   
}

