##the "makeCacheMatrix" function creates a special "matrix" object,
##taking an invertible matrix and caching the inverse
##"cacheSolve" retrieves the inverse of the special matrix if 
##previously computed,
##computes the inverse if not found or special "matrix" changed

makeCacheMatrix <- function(x = matrix()) { #
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x #"get" function returns matrix
        setinverse <- function(solve) m <<- solve #
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data") #prints "getting cached data" 
							     #if inverse previously computed 
							     #and cached
                return(m) #prints inverse matrix if previously computed and 
				  #cached
        }
        data <- x$get()#retrieves matrix
        m <- solve(data, ...) #if not previously computed and changed, computes inverse
        x$setinverse(m)
        m #print inverse matrix
}