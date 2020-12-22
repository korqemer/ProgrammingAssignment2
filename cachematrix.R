## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix create R object that stores initial matrix and its inverse
## it is done to save time while finding the inverse

makeCacheMatrix <- function(initMatrix = matrix()){
    inv <- NULL
    set <- function(y){
        initMatrix <<- y
        m <<- NULL
    }
    get <- function(){
        initMatrix
    }
    setInv <- function(inverse){
        inv <<- inverse
    }
    getInv <- function(){
        inv
    }
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve gets inverse of initial matrix, and if it doesn't exist, calculates and stores in x

cacheSolve <- function(x, ...){
    m <- x$getInv()
    if (!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}