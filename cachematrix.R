## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    inver <- NULL
    
    #Set the value of the matrix
    set <- function(val){
        x <<- val
        inver <<- NULL
    }
    
    #Get the value of the matrix
    get <- function(){
        x
    }
    
    #Set the value of inverse of the matrix
    setiverse <- function(inverse){
        inver <<- inverse
    }
    
    #Get the value of inverse of the matrix
    getinverse <- function(){
        inver
    }
    
    list(
        set = set,
        get = get,
        setiverse = setiverse,
        getinverse = getinverse
    )
    
}


## This function returns the inverse of the matrix.
## It checks if the inverse has already been calculated, 
## then the function retrieves the inverse matrix from the cache.
## Otherwise it computes the inverse.

cacheSolve <- function(x, ...) {
    
    inver <- x$getinverse()
    if(!is.null(inver)){
        message("getting cached data")
        return(inver)
    }
    
    data <- x$get()
    inver <- solve(data)
    x$setiverse(inver)
    
    inver
    
}