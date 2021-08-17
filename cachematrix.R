## makeCacheMatrix function creates a special matrix object which also allows
## to store/cache inverse of the original matrix as well. It also hosts four 
## function inside a list that allow for setting and accessing values
## related to the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Initializing the variable that stores the inverse of matrix
    mat_inv  <- NULL
    
    ## The following are the 4 functions that are used to 
    ## alter/set the actual matrix values and the inverse matrix.
    set <- function(y) {
        x <<- y
        mat_inv <<- NULL
    }
    get <- function() x
    set_inv <- function(inv) mat_inv <<- inv
    get_inv <- function() mat_inv
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## cacheSolve function first checks if the provided matrix
## already has its inverse calculated and stored in cache. If so, 
## it will avoid calculating the inverse of the matrix and instead
## it will return the stored value for the inverse. If there is no
## inverse store in cache, it will then calculate the inverse and
## also stores(i.e. sets) the inverse value in cache for future 
## retrievals

cacheSolve <- function(x, ...) {
    ## This function returns a matrix that is the inverse of 'x'
    
    ## Firstly, the functions check to see whether there is already a value for
    ## the inverse of the matrix 'x'. If so, it will return the already existing
    ## inverse matrix.
    matrix_inv <- x$get_inv()
    if(!is.null(matrix_inv)) {
        message("getting cached data")
        return(matrix_inv)
    }
    
    ## If there is not already a value for the inverse of matrix 'x', then the
    ## inverse matrix is calculated from the original matrix 'x' and then stored
    ## and ultimately returned by the function
    mat_1 <- x$get()
    matrix_inv <- solve(mat_1, ...)
    x$set_inv(matrix_inv)
    matrix_inv
}
