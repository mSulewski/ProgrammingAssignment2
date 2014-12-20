## Overall description of what the functions do:
# The use of these two functions allow us to cache the inverse of a matrix which can be a time-consuming
# computation, especially as a matrix gets larger. Once cached, we may easily grab it later rather than 
# recompute it when necessary. Both the matrix and its inverse values can be updated using the superassignment.


# makeCacheMatrix allows us to read and change the matrix that is passed in as well as the matrix inverse 
# and creates a list object containing these functions.
makeCacheMatrix <- function(x = matrix()) {
    # setting inverse to NULL
    i <- NULL
    
    # get/read the matrix
    get <- function() { x }
    
    # set/change the matrix if updated
    set <- function(y) {
        x <<- y     # changing the matrix using superassignment
        i <<- NULL  # resetting inverse back to NULL using superassignment
    }
    
    # get/read the inverse matrix
    getinverse <- function() { i }
    
    # set/change the inverse matrix
    setinverse <- function(inverse) { i <<- inverse } # store the inverse in 'i' using superassignment
    
    # list containing the above functions
    list(get = get, set = set, getinverse = getinverse, setinverse = setinverse)
    
}

# cacheSolve computes the inverse of a matrix.
# If the inverse has already been calculated, cached and not changed, then it is returned
# otherwise, it calculates the inverse, caches and then returns it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    # accesses object 'x' and gets the inverse
    i <- x$getinverse()  
    
    # checks if the inverse of a matrix is already cached (not NULL)
    if(!is.null(i)) {  
        message("getting cached data")
        return(i)  # returns the cached inverse of a matrix
    }
    
    # if inverse is not cached then it gets the values of the matrix
    data <-x$get() 
    
    # inverses the matrix and stores the result in 'i'
    i <- solve(data)
    
    # stores the newly calculated inverse in x object
    x$setinverse(i)
    
    # returns the inverse of the matrix
    i
}
