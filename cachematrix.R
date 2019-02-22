## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function Function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
#get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object
#can cache its own object. 

#<<- operator is used to assign a value to an object in an environment that is different 
#from the current environment 

#take the matrix as an input
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  
  #set the value of the Matrix
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  getMatrix <- function() x                              
  setInverse <- function(inverse) invMatrix <<- inverse  
  getInverse <- function() invMatrix                     
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
  
}


## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
# In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
# and set the invertible  matrix by using the solve function.
# In case inverse matrix from makeCacheMatrix((matrix) has some value in it (always works
#after running the code 1st time), it returns a message  "Getting Cached Invertible Matrix" 
#and the cached object



cacheSolve <- function(x, ...) {
  
  #get the value of the invertible matrix from the makeCacheMatrix function
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {                       
    message("Getting Cached Invertible Matrix")    
    return(invMatrix)                           
  }
  
  #if value of the invertible matrix is NULL then  
  MatrixData <- x$getMatrix()                     
  invMatrix <- solve(MatrixData, ...)             
  x$setInverse(invMatrix)                         
  return(invMatrix)                             
  ## Return a matrix that is the inverse of 'x'
}



####Test Matrix #####
TestMatrix <- matrix(1:4,2,2)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)



TestMatrix <- matrix(c(1,5,8,2),2,2)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)



TestMatrix <- matrix(1:9,3,3)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)

####Test 4 [3*3 Matrix]#####
TestMatrix <- matrix(1:8,3,3)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)


TestMatrix <- matrix(c(2,3,5,1,3,7,4,5,6,8,0,0,4,5,6,0),4,4)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)


TestMatrix <- matrix(5:21,4,4)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)
