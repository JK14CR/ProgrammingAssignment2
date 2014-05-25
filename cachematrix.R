##Functions for inversing and caching matrix inverse 


## Function makeCacheMatrix creates a matrix object and stores its inverse 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
      set <- function(y) {		#sets values the matrix object 
                x <<- y
                m <<- NULL
        }
        get <- function() x		#returns the matrix object 
        setinv <- function(inverse) m <<- inverse	#sets values of the inverse matrix
        getinv <- function() m	#returns the inverse matrix 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Function cacheSolve calculates an inverse of a given matrix and stores the inverse in
## the cache. If called again, the function checks the cache and retrieves the inverse, if
## it has been already calculated, otherwise calculates the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinv()	#checks if the inverse matrix was already calculated (that is not NULL)
        if(!is.null(m)) {
                message("getting cached data")
                return(m)	#returns the inverse matrix
        }
        data <- x$get()		#to calculate the inverse of x,the function gets the matrix x
        m <- solve(data, ...)		#uses solve function for inverse
        x$setinv(m)		#sets values of the inverse matrix in x 
        m
}
