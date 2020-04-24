



## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a fiunction that will store a given matrix 
## and can return its inverse.

##makeCacheMatrix: This function creates a special "matrix" 
##object that can cache its inverse.

makeCacheMatrix <- function(matrix = matrix()) {  ##Create founction that take one arg= matrix)
    
    inverted_matrix <- NULL                ## Create "invert" an empty value for now but will
    ## store the inverted matrix when founction completed
    setmatrix <- function(newmatrix) {    ## setMatrix is a founction that will set the "new" matrix given
        matrix <<- newmatrix              ##and errased the old value for "invert" if it had one
        inverted_matrix <<- NULL
    }
    getmatrix <- function() {           ## Return the matrix we were provided.
        matrix
    }
    
    setinvert <- function(new_invert){
        inverted_matrix <<- new_invert          ## setinvert set the invert of the Matrix
    } 
    getinvert <- function() inverted_matrix ## getinvert return the invert of the Matrix
    
    list(setmatrix = setmatrix, getmatrix = getmatrix,    ## create the list with how to get the matrix, 
         getinvert = getinvert, setinvert = setinvert)   ##
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
    inverted_matrix <- x$getinvert()
    if(!is.null(inverted_matrix)) {
        message("getting cached data")
        return(inverted_matrix)
    }
    data <- x$getmatrix()
    inverted_matrix <- solve(data, ...)
    x$setinvert(inverted_matrix)
    inverted_matrix
}



berry<-matrix(c(2,0,0,1), nrow = 2, ncol=2)
berry
solve(berry)
apple <- makeCacheMatrix(berry)
apple$getmatrix()
apple$getinvert()
cacheSolve(apple)
apple$getinvert()