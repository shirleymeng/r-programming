# makeCacheMatrix returns a functions lists
#	input: a matrix
#	output: a list of 4 functions:
#		get(): get the matrix
#		set(): set the matrix
#		setivsmatrix(): convert the input matrix into a inverse matrix
#		getivsmatrix(): get the inversed matrix				     		
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


# cacheSolve function returns the inverse matrix
# 	input: a function list created from makeCacheMatrix
#	output: inversed matrix either get from cache
#		  or generated from the input object

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