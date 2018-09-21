## Programming Assignment R Programming Week 3 - Part 2
##
## You get the inverse of a matrix. Because matrix inversion is a costly
## computation this R function will check whether the inverse has already been
## calculated and saved in the cached. If so, it will get from there. If not, it
## will recalculate it. 
##
## This assignment is based on the 'cachemean.R' file.
## 
## The first function, makeCacheMatrix creates a special "matrix", which is
## really a list containing a function to
## 
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse of the matrix
## - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse
        )
}


# The following function calculates the inverse of the special "matrix" created
# with the above function. However, it first checks to see if the inverse has
# already been calculated. If so, it gets the inverse from the cache and skips
# the computation. Otherwise, it calculates the inverse of the data and sets the
# inverse in the cache via the setinverse function.



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
