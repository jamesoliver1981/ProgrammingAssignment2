##makeCacheMatrix creates storage for inversed matricies
##cacheSolve makes use of above to save processor time

##these functions use R's lexical scoping rules to set functions
##outside of their own environment to speed up computation


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                       #set m to NULL - used in CacheSolve if nothing found
        set <- function(y) {
                x <<- y                                 #create variables available outside own environment
                m <<- NULL
        }
        get <- function() x                             #sets matrix of x
        setmatrix <- function(solve) m <<- solve       #inverts matrix using solve -
       						 #also stores inverse matrix - ref'd in cacheSolve
        getmatrix <- function() m                       #returns inverted matrix
        
        list(set = set, get = get,                      #generate the created functions to working env.
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}
cacheSolve <- function(x, ...) {
        m <- x$getmatrix()                      #set m as returned value from makeCacheMatrix
 					        #if not created, will return NULL as set above
        if(!is.null(m)) {
                message("getting cached data")  #if m calc'd, generate msg& return m
                return(m)
        }
        matrix <- x$get()                       #else get matrix set to matrix
        m <- solve(matrix, ...)                 #invert matrix and set as m
        x$setmatrix(m)                          #store m in makeMatrixCache
        m                                       # return m
}

