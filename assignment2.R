makeCacheMatrix <- function(x = matrix()) {
        ivsmatrix <- NULL
        set <- function(y) {
                x <<- y
                ivsmatrix <<- NULL
        }
        get <- function() x
        setivsmatrix <- function(solve) m <<- solve(x)
        getivsmatrix <- function() m
        list(set = set, get = get,
             setivsmatrix = setivsmatrix,
             getivsmatrix = getivsmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
	  m <- x$getivsmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setivsmatrix(m)
        m
}