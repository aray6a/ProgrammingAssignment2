## Functions used to make a matrix environment and
## cache the matrix inverse

## Create an environment containing a matrix object
makeCacheMatrix <- function(x = matrix()) {
    #Making environment
    m <- NULL
    
    #Create functions
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolved <- function(solved) m <- solved
    getsolved <- function() m
        
    #Returning functions
    list(set = set, get = get, setsolved = setsolved,
         getsolved = getsolved)
    
}


## Return a matrix that is the inverse of 'x'.  'x' must
## be a cached matrix created by makeCacheMatrix
cacheSolve <- function(x, ...) {
    #Get cached inverse    
    solved <- x$getsolved()
    
    #If cached inverse is not null, we're done
    if( !is.null(solved) ) {
        return(solved)
    }
   
    #Otherwise, get matrix, solve for inverse, cache the
    #answer
    mtrix <- x$get()
    solved <- solve(mtrix)
    x$setsolved(solved)
    solved
}
