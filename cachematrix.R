## Function to calculate the inverse matrix with an ability not to make 
## doubled calculations if they has already been done - just getting cached inverse matrix instead

## function to create an object to store matrix and it's inverse  matrix in case it has already been calculated

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## function returns cached inverse matrix, if it has alredy been calculated and calculating the inverse in the opposite case

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}
