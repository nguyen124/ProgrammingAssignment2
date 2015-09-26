## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will receive a matrix as input then return a list of functions associating with this input
makeCacheMatrix <- function(data = matrix()) {
  cacheMatrix <- NULL
  ## Getter and Setter for data
  setData <-function(y)
  {
    data <<-y
    cacheMatrix<<-NULL
  }
  getData <-function(){
    data
  }
  ## Getter and Setter for cacheMatrix
  setMatrix<-function(m){
    cacheMatrix <<-m
  }
  getMatrix <-function(){
    cacheMatrix
  }
  ## return list of functions
  list(setData=setData,getData=getData,setMatrix=setMatrix,getMatrix=getMatrix)
}


## Write a short comment describing this function
## This function is used the list of functions as input to get the invert of matrix. 
## If the matrix has been calculated, the invert matrix won't be calculated again
cacheSolve <- function(functionsList, ...) {
  
  ## get the matrix, if cacheMatrix is not null it means the matrix has been calculated for the inverse matrix
  ## so we dont have to calculate it again
  cacheMatrix<-functionsList$getMatrix()
  if(!is.null(cacheMatrix)){
    message("getting cached matrix")
    return (cacheMatrix)
  }
  ## if the cacheMatrix is null, let calculate the inverse of matrix 
  data<-functionsList$getData();
  inverMatrix<- solve(data)
  functionsList$setMatrix(inverMatrix)
  ## Return a "inverse matrix" that is the inverse of 'matrix'
  inverMatrix
}

## Testing function on R console
## Step1: create invertible Square Matrix 
## matrix2 <- matrix(c(4,2,7,6),2)
## matrix2
##       [,1] [,2]
## [1,]    4    7
## [2,]    2    6
##
## Step2: intialize functions list with matrix2 as input data
## functionsList <- makeCacheMatrix(matrix2)  
##
## Step3: solve the maxtrix2 to get invert matrix of it
## invertMatrix2<-cacheSolve(functionsList)
## invertMatrix2
##       [,1] [,2]
## [1,]  0.6 -0.7
#3 [2,] -0.2  0.4
##
## Step4: run the solve again to get invertmatrix from cache
## invertMatrix2<-cacheSolve(functionsList)
## getting cached matrix
##
## Step 5: double check the invert matrix by multiplying both matrix
## matrix2 %*% invertMatrix2
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
##
## if the result is an identity matrix, we've done correctly
