    ##The Program here will read a matrix and give out the inverse of the matrix
    ## Cached value is given if the inverse was calcualted before for the same matrix
    

    ## The makeCacheMatrix Fucntion is just taking the input of the matrix and giving the output as lit of functions
    makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
    }



## The cacheSolve function is evaluating the presence of the inverse matrix in the cache  and printing it out or is calculating the inverse in the absence of it
cacheSolve <- function(x = matrix(), ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")   
        return(m) ## This part returns the cached matrix
    }
    message("First time calculated data")
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)  ## This part returns the calculated matrix
    m
} 
