## Calculating inverse is a time consuming job. So to avoid multiple times the
## calculation of an inverse of a same matrix we store inverse on cache and next
## time when it is called we just read from the cache and returns the inverse

##  makeCacheMatrix creates a matrix object which also stores the inverse of 
##  the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- matrix()
        set <- function(y){
        x <<- y
				        inv <<- matrix()
        }
        get <- function() x
		    setinv <- function(inverse) inv <<- inverse
		    getinv <- function() inv
		    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve checks if the matrix inverse is stored on cache or not and if yes
## it outputs inverse and if not it calcultes the inverse and stores the inverse
## on cache and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!any(is.na(inv))){
		            message("getting cached data")
                return(inv)
        }
        data <- x$get()
		    inv <- solve(data,...)
		    x$setinv(inv)
		    inv
}