## There are two functions in the script, which makes the usage of matrix inverses
## faster by applying a cache. makeCacheMatrix makes and handles the inverse
## calculation and caching and cacheSolve is a wrapper that can be used as an
## interface for the other function.


## makeCacheMatrix is a function that handles and calculates the inverse
## of a matrix
##  -> input: x, the matrix; default value: an empty matrix
##  <- output: a list with the matrix and its caching functions
makeCacheMatrix <- function(x = matrix())
{
    # a variable which will be used as the cache
    inverse <- matrix()
    
    # a function which assigns a new value to x and deletes cache
    #   -> input: the new matrix
    setX <- function(y)
    {
        x <<- y
        inverse <<- matrix()
    }
    
    # a function which returns the current value of the cache
    #   <- output: the original matrix
    getX <- function()
    {
        x
    }
    
    # a function that stores the saves the new inverse to the cache
    #   -> input: the new inverse
    setInverse <- function(inv)
    {
        inverse <<- inv
    }
    
    # a function that stores the returns the inverse from the cache
    #   <- output: the cached inverse
    getInverse <- function()
    {
        inverse
    }
    
    # returning the extended functions of the matrix as a vector
    list(setX = setX, getX = getX, setInverse = setInverse,
         getInverse = getInverse)

}


## cacheSolve is wrapper-function that handles the cached inverse of a matrix
##  -> input: x, the matrix
##  <- output: the inverse of x
cacheSolve <- function(x, ...)
{
    
    # if the argument is not a cachable matrix, jut a matrix then
    # it has to be modified and the inverse returned
    if(typeof(x) != "list")
    {
        # a cacheable matrix is assigned to the original variable
        assign(x = deparse(substitute(x)), 
                        value = makeCacheMatrix(x),
                        pos = .GlobalEnv)
        
        # and the inverse is returned
        return(solve(x))
        
    }
    
    # getting the inverse from the cache
    inv <- x$getInverse()
    
    # two logical variables shoiwing whether the cache was empty
    # the cache is empty if it has the length 1
    # and the only element is NA
    emptyInverse <- isTRUE(all.equal(dim(inv), c(1, 1)))
    naInverse <- is.na(inv[1, 1])
    
    # if the inverse was cached, the value in the cache is returned
    if(!(emptyInverse && naInverse))
    {
        print("returning value from cache")
        return(inv)
    }
    
    # if the cache was empty, the matrix has to be read,
    # the inverse calculated, saved and returned
    # calculating the inverse 
    currMat <- x$getX()
    x$setInverse(solve(currMat))
    x$getInverse()
}
