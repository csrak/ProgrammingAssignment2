
## makeCacheMatrix gets a matrix and returns a list containing 4 functions.
## set: Changes the value of the matrix and resets previous inverse calculations.
## get: Gives the value of the matrix.
## setinverse: sets the inverse of the matrix as the value entered.
## getinverse: Gives the value of a previously calculated inverse, NULL if none.
## when called makeCacheMatrix also sets the calculated inverse as NULL. 

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x 
                
        setinverse <- function(inverse) m <<- inverse         
                
        getinverse <- function() m
                
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) 
        ## Returns a list that allows cacheSolve to work properly and allows you 
        ## to use the functions created on your matrix.
      
}

## Note1: cacheSolve needs makeCacheMatrix to be called first to work properly.
## cacheSolve checks if the inverse of the matrix was calculated previously, 
## if the inverse is already calculated it tells the user,then  gives him the 
## value and skips the calculation to save time.
## Else it gets the value of the matrix, calculates the inverse using "Solve()"
## then sets the value of the inverse to avoid future calculations and finally 
## returns the value of the inverse.

cacheSolve <- function(x, ...) {
        m <- x$getinverse() 
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m) ## Returns a previosuly calculated matrix that is the 
                          ##inverse of 'x'.
        }
        
        data <- x$get()
        
        m <- solve(data, ...)
        
        x$setinverse(m)

        m        ## Return a matrix that is the inverse of 'x'.
}
