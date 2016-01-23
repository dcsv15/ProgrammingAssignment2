## makeCacheMatrix - Creates a special "matrix" object that can cache its inverse.
## The object does not calculate the inverse, just saves it inside.
## Saves the matrix to variable x and its inverse to variable s in scope.
## Returned object (actually it's a list) contains functions as below:
## set: sets matrix and resets cached inverse
## get: returns matrix
## setInverse: saves solve value
## getInverse: returns cached inverse value

makeCacheMatrix <- function(x = matrix()) {

    returnMatrix <- NULL
    set <- function(matrixStore)
    {
        x <<- matrixStore 
        returnMatrix <<- NULL            
    }
    get <- function() x
    setInverse <- function(solve) returnMatrix <<- solve
    getInverse <- function() returnMatrix
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)

}


## cacheSolve - Function to get the inversed matrix from a special object created by makeCacheMatrix.
## Takes the object of that type as an argument 'x', 
## checks if the inverse value is already cached,
## and if it is cached, then returns the cached value; 
## if not, this function calculates the inverse for the matrix saved in the 'x', 
## saves it into 'x' cache using method 'setInverse'
## and returns the result.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	matrixStore <- x$getInverse()
	if(!is.null(matrixStore))
	{
	   message("Getting Already Cached Matrix and Return It")
	   return(matrixStore)
	}
	else
	{
	  ##No cached value, so calculate inverse and cache it, before return.
	  message("First Time: No Cached Matrix, calculate Inverse, cache and return.")
	  data <- x$get()
	  matrixStore <- solve(data, ...)
	  x$setInverse(matrixStore)
	  matrixStore
	}
        
}

#Unit Test and result verification as follows
#Commented as this was just for testing purpose only
#x <- matrix( c(2, 4, 3, 1, 5, 7, 1, 2, 3),  nrow=3, ncol=3) 
#xm <- makeCacheMatrix(x)
#xs <- cacheSolve(xm)
#xs
#xIsCache <- cacheSolve(xm)
#xIsCache