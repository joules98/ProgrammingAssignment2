## The set of functions "makeCacheMatrix" and "cacheSolve" will invert an invertible matrix, 
## return the results, and store the results into cache so that they may be recalled, if needed,
## and save a potentially costly repeat computation.
## 
## The makeCacheMatrix function accepts an invertible matrix and creates a special "matrix" object that, 
## together with the cacheSovlve fuction, can cache the inverse of a matrix.  Also, makeCacheMatrix  
## creates a list of functions that are called by the cacheSolve function.
makeCacheMatrix <- function(xx = matrix()) {
        inv <- NULL                             ## Reset the inv matrix to null to clear the current environment.
        set <- function(yy) {                   ## Create the set function to leverage the lexical scoping property in R.
                xx <<- yy                       ## Assign the input matrix to an matrix in an environment that is different 
                inv <<- NULL                    ## from the current environment; reset inv matrix to NULL in that environment.
        }
        get <- function() {xx}                  ## Return the input matrix.
        setinverse <- function(solve) {         ## Create the setinverse function to leverage the lexical scoping property in R.
                inv <<- solve                   ## Save the inverse returned from the cacheSolve function to the other environment - "cache".
        }
        getinverse <- function() {inv}          ## Obtain the inverse matrix from cache (if it exists).
                                                ## Create a list with the names of the fumctions available to cacheSolve.
        list(set = set, get = get,              
             setinverse = setinverse,           
             getinverse = getinverse)          
}
## The cacheSolve function computes the inverse of the special "matrix" returned by the makeCacheMatrix function above.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve function will
## retrieve the inverse from the cache, thereby potentially saving computation time. If the inverse has not been calculated, 
## then the function will calculate and return the inverse of the matrix and store it into cache so that it can 
## be returned again, if needed.
cacheSolve <- function(xx, ...) {
        inv <- xx$getinverse()                  ## Obtain the inverse matrix from cache, if it exists in cache.
        if(!is.null(inv)) {                     ## Check to see if the inverse is already stored in cache.
                message("getting cached data")  ## If yes, then return the message "getting cached data".
                return(inv)                     ## Return the inverse matrix obtained from cache.
        }
        data <- xx$get()                        ## This will only run if the inverse matrix was not already in cache. Obtain input matrix.
        inv <- solve(data)                      ## Calculate the inverse of the input matrix.
        xx$setinverse(inv)                      ## Send the resulting inverse matrix to cache.
        inv                                     ## Print the inverse of the input matrix.
}
