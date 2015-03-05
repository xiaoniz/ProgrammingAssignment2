##ProgrammingAssignment2
##  This assignment is to write a pair of functions that cache the inverse of a matrix. 
##  Two functions are written: makeCacheMatrix and cacheSolve.This purpose of the 
##  functions is to improve computation efficiency.

## The makeCacheMatrix function contains four functions: set, get, setInvm, and getInvm.
## The <<- operator is used to assign a value to an object in an environment that is different
## from the current environment. 

makeCacheMatrix <- function(x = matrix()) {
    xinvm <- NULL # store inverse matrix
   
    set <- function(y) {
        x <<- y
        xinvm <<- NULL # set xinvm to null
    }
    
    get <- function() x # return the input matrix 
    setInv <- function(inv) xinvm <<- inv # set the inverse matrix
    getInv <- function() xinvm # return the inverse matrix
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
    
}


## The cacheSolveWrite function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated, 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInv() # get the inverse matrix of x
   
    if(!is.null(m)) { # If the inversion result is computed
        message("get cached data")
        return(m) # return the calculated inverse matrix
    }
    
    data <- x$get() # Not in cache we do x$get to get the matrix object
    m <- solve(data) # Solve the matrix
    x$setInv(m) # set the object in cache
    m # return the solved result
    message("compute new data")

}

# Test
# generate a random square integer matrix
test <- matrix(sample.int(81, size = 9*9, replace = TRUE), nrow = 9, ncol = 9) #generate a square matrix 9*9
# generate the makeCacheMatrix object with this matrix
testCached <- makeCacheMatrix(test)
# from now on calculate or retrieve calculated inversion using the cacheSolve function

testInv <- cacheSolve(testCached)
testInv <- cacheSolve(testCached)
