## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    # set the value of the matrix    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # get the value of the matrix    
    get <- function() x
    # set the value of the inverse of the matrix   
    set_inv <- function(new_inverse) inverse <<- new_inverse
    # get the value of the inverse of the matrix   
    get_inv <- function() inverse
    list(set=set, 
         get=get, 
         set_inv=set_inv, 
         get_inv=get_inv)
}


cacheSolve <- function(x, ...) {
    inv <- x$get_inv()
    # check if the inverse matrix has been solved 
    if(!is.null(inv)) {
        # yes, retrieved from cache
        message("getting cached data.")
        return(inv)
    }
    # else, get the matrix data 
    data <- x$get()
    # solve the matrix
    inv <- solve(data)
    # set it in cache
    x$set_inv(inv)
    inv

}
