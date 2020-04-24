
## makeCacheMatrix is a fiunction that will store a given matrix 
## and can return its inverse.

##makeCacheMatrix: This function creates a special "matrix" 
##object that can cache its inverse.

makeCacheMatrix <- function(matrix = matrix()) {  # Create founction that take one arg = matrix)
    
    inverted_matrix <- NULL                # Create "inverted_matrix" an empty value for now but will
                                           # store the inverted matrix when founction cacheSolve completed
    setmatrix <- function(newmatrix) {     # setMatrix is a founction that will set the "new" matrix given
        matrix <<- newmatrix               # and errased the old value for "invert" if it had one
        inverted_matrix <<- NULL
    }
    getmatrix <- function() {             # Return the matrix we were provided.
        matrix
    }
    
    setinvert <- function(new_invert){
        inverted_matrix <<- new_invert          # setinvert set the invert of the Matrix
    } 
    getinvert <- function() inverted_matrix     # getinvert return the invert of the Matrix
    
    list(setmatrix = setmatrix, getmatrix = getmatrix,    # create the list with how to get/set the matrix, 
         getinvert = getinvert, setinvert = setinvert)    #and get/set inverted_matrix
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {        # founction to return the inverted of a matrix 
    inverted_matrix <- x$getinvert()    # check if it was already stored if yes 
    if(!is.null(inverted_matrix)) {     # say "getting cached data" and show the inverted 
        message("getting cached data")
        return(inverted_matrix)         # return mean end the function if is getting a result so
    }                                   # do not run the second part but if if is NULL run data<-...
    data <- x$getmatrix()
    inverted_matrix <- solve(data, ...)
    x$setinvert(inverted_matrix)
    inverted_matrix
}

# exemple to see if it works

berry<-matrix(c(2,0,0,1), nrow = 2, ncol=2) #create a matrix
berry
solve(berry)
apple <- makeCacheMatrix(berry)             # make the cacheamtrix of the matrix berry
apple$getmatrix()                           # get the matrix
apple$getinvert()                           # get the inverte of the matrix but should be NULL because not calculated yet 
cacheSolve(apple)                           # get the inverted_matrix from cache
apple$getinvert()                           # get the inverted from makecahe now that is was calculate