makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
    
}



cacheSolve <- function(x, ...) {
    inverse_matrix <- x$getinverse
    if(!is.null(inverse_matrix)){
        message("getting cached data")
        return(inverse_matrix)
    }
    
    original_matrix <- x$get()
    inverse_matrix <- solve(original_matrix,...)
    x$setinverse(inverse_matrix)
    inverse_matrix
}
