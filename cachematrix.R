## Ram Mohan Chandran - 27/Aug/2020

## Simple R functions to exhibit caching data from complex computations
## using R's lexical scoping


## Caching function using lexical scoping
## Returns list of functions get,set,getInverseMatrix & setInverseMatrix 
## for every valid invertible matrix 

makeCacheMatrix <- function(x = matrix()) {
  
  #Init the "to be cached inv matrix" variable as null 
  #This is to avoid transient data/from previous execution going to different matrix 
  invmatrix <- NULL
  
  #set invertible  matix and init inv matrix property to null
  set <- function(new_matrix) {
    x <<- new_matrix
    invmatrix <<- NULL
  }
  
  #Get orignal matrix
  get <- function() x
  
  #set inverted matrix to cache
  setInverseMatrix <- function(new_invmatrix) invmatrix <<- new_invmatrix
  
  #get inverted matrix from cache
  getInverseMatrix <- function() invmatrix
  
  #returns list of functions to operate on for every invertible  matrix
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
  
  
}


## Function to resolve inverted matrix from cache else set in cache
## x to be of function type makeCacheMatrix
## test :- 
##      a <- makeCacheMatrix(matrix(1:4,2,2))
#       cacheSolve(a)
#       NULL    # computes inverse when data not cache
#       [,1]            [,2]
#       [1,] 0.3333333 -0.1666667
#       [2,] 0.0000000  0.5000000
#     > cacheSolve(a)
#       getting cached data   # gets inverse from cache
#       [,1]            [,2]
#       [1,] 0.3333333 -0.1666667
#       [2,] 0.0000000  0.5000000
#       
##
##

cacheSolve <- function(x, ...) {
  
  ## check cache for inverse for the matrix held in x
  invertedMatrix <- x$getInverseMatrix()
  
  #Not null check for inverse data
  if(!is.null(invertedMatrix)) {
    message("getting cached data")
    return(invertedMatrix)
  }
  #print(invertedMatrix)
  
  ## Data not cached , original matrix is sourced using functions from
  ## the list returned from makeCacheMatrix function
  originalMatix <- x$get()
  
  #compute inverse matrix using solve function
  invertedMatrix <- solve(originalMatix)
  
  #set the computed inverse against original matrix via functions offered by  makeCacheMatrix
  x$setInverseMatrix(invertedMatrix)
  
  #return inverted matrix
  invertedMatrix

}
