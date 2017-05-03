## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Function to cache the input matrix
## Returns special list of methods to use the cache
makeCacheMatrix <- function( mtx=matrix() ) {
    inv_mtx <- NULL
    set <- function(in_mtx) {
        mtx <<- in_mtx
        inv_mtx <<- NULL
    }
    get <- function() {
        mtx
    }
    setinv <- function(slv) {
        inv_mtx <<- slv
    }
    getinv <- function() inv_mtx
    list( set=set, get=get, setinv=setinv, getinv=getinv)
}

## Write a short comment describing this function

## Function takes the special list as input
## Return a matrix that is the inverse of 'mtx'
cacheSolve <- function(i) {
    inv_mtx <- i$getinv()
    if(!is.null( inv_mtx )) {
        message("getting cached data")
        return(inv_mtx)
    }
    data <- i$get()
    inv_mtx <- solve(data)
    i$setinv( inv_mtx )
    inv_mtx
}
