## The functions used together will calculate the inverse of a matrix in the inverse has not already been calculated. If it is calculated and cached in the memory, the functions will retrieve it
## The functions need to be used together, makeCacheMatrix first caches the matrix and cacheSolve solves or retrieves the solution if the inverse is new or cached respectively
## for example for a 2 x 2 matrix x <- matrix(c(1:4),2,2), using 

## makeCacheMatrix first caches the matrix but does not solve it. It stores the inverse solution after cacheSolve is called on it 

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


## cacheSolve first searches makeCacheMatrix for stored solutions, if a solution is stored under makeCacheMatrix$getinverse, the solution is retrieved, if not, cacheSolve calculates the inverse, returns the result, and stores the inverse in makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
