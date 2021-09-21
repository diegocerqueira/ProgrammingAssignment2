## makeCacheMatrix:
## This function creates a special cacheable matrix. It returns a special list 
## with the functions:
## 1. setMatrix - it sets the value of the matrix that needs to be solved 
##    (i.e. inverted) and cache it;
## 2. getMatrix - this function returns the matrix cached in 'x'
##         It will return an empty matrix of 1 row/column as NA if not previously
##        initialized by 'setMatrix'.
## 3. setInvertedMatrix - this function caches an inverted matrix (i.e. already
##    solved by the function 'cacheSolve')
## 4. getInvertedMatrix - this function returns a cached inverted matrix
##        It will return NULL if the matrix has not been solved yet.

makeCacheMatrix <- function(x = matrix()) {
  inverted.matrix <- NULL 
  setMatrix <- function(matrix.to.cache) {
    x <<- matrix.to.cache
    inverted.matrix <<- NULL
  }
  getMatrix <- function() {
    x
  }
  setInvertedMatrix <- function(solved.matrix) {
    inverted.matrix <<- solved.matrix
  }
  getInvertedMatrix <- function() {
    inverted.matrix
  }
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInvertedMatrix = setInvertedMatrix,
       getInvertedMatrix = getInvertedMatrix)
}

## cacheSolve:
## This function returns a matrix that is the inverse of the matrix 'x' supplied 
## as an argument of this function.
## The matrix 'x' is a special cacheable matrix that must be initialized with 
## the function 'makeCacheMatrix'.
## If the matrix has been already solved, the result will be returned from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  solved.matrix <- x$getInvertedMatrix()
  if(!is.null(solved.matrix)) {
    message("getting cached data")
    return(solved.matrix)
  }
  matrixData <- x$getMatrix()
  solved.matrix <- solve(matrixData)
  x$setInvertedMatrix(solved.matrix)
  solved.matrix
}

## Usage
## 1. Create a square invertible matrix object;
## 2. Cache it using the 'makeCacheMatrix', e.g.:
##       cached.matrix <- makeCacheMatrix(square.matrix)
## 3. Solve it (i.e. invert it) and cache the results using the function
##    'cacheSolve', e.g.:
##       cacheSolve(cached.matrix)
##    The first time it runs, it will cache the result, the next time it will 
##    retrieve the cached data from the memory.

## Testing suggestion:
## 1. Create a square invertible matrix object, e.g.:
##          m <- matrix(1:4, nrow = 2, ncol = 2)
## 2. create the special cacheable matrix, using the function makeCacheMatrix, e.g.:
##          cm <- makeCacheMatrix()
##  This command will not initialize (i.e. set) the matrix yet, alternatively, 
##  you can call this function passing the 'm' object as an argument:
##          cm <- makeCacheMatrix(m)
## 3. Check if the matrix 'm' is cached, e.g.:
##          cm$getMatrix()
##       If the matrix has not been initialized yet (e.g. no argument passed to
##       the 'makeMatrix' function) it will return an empty matrix (1x1) with NA.
## 3. Initialize the matrix (if you have not done so passing the argument to
##    function 'makeCacheMatrix'), e.g.:
##          cm$setMatrix(m)
##    Note that we are doing this for testing purposes only. The normal usage of 
##    'makeCacheMatrix' is with its argument - it is not desireable to access the 
##     list elements directly (using the operator '$').
## 4. Verify that the matrix has been successfully set, e.g.:
##         cm$getMatrix()
## 5. Solve the matrix for the first time, to cache the results:
##        cacheSolve(cm)
## 6. Run the same command again, to verify that now the result is retrieved
##    from cache.
## 7. Just to check that values are correctly cached in the cacheable matrix
##    object, access the value 'getInvertedMatrix' of object 'cm' using the '$'
##    operator, e.g.:
##         cm$getInvertedMatrix()