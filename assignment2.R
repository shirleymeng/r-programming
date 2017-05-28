makeCacheMatrix <- function(x = matrix()) {
        ivsmatrix <- NULL
        set <- function(y) {
                x <<- y
                ivsmatrix <<- NULL
        }
        get <- function() x
        setivsmatrix <- function(solve) ivsmatrix  <<- solve(x)
        getivsmatrix <- function() ivsmatrix 
        list(set = set, get = get,
             setivsmatrix = setivsmatrix,
             getivsmatrix = getivsmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
	  ivsmatrix  <- x$getivsmatrix()
        if(!is.null(ivsmatrix)) {
                message("getting cached data")
                return(ivsmatrix)
        }
        data <- x$get()
        ivsmatrix <- solve(data)
        x$setivsmatrix(ivsmatrix )
        ivsmatrix 
}