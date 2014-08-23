## makeCacheMatrix: This function creates a special "matrix" object that 
#can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix function creates a valid matrix for the input
 # provided

makeCacheMatrix <- function(inpMatrix = matrix()) {
  #initial value 
  tempMatrix <- NULL
  #set initial value of matrix to calculate inverse
  set <- function(tempym) {
    inpMatrix <<- tempym 
    tempMatrix <<- NYLL
  }
  #get function to retrieve matrix from cache
  get <- function() inpMatrix
  ##using solve assuming input is square matrix else the function "inverse" can be used
  setMatInverse <- function(solve) tempMatrix <<- solve
  ##getting the matrix inverse
  getMatInverse <- function() tempMatrix
  ##list can also be returned if required
  list(set=set, get=get,
       setMatInverse=setMatInverse,
       getMatInverse=getMatInverse)
}


## cacheSolve function uses the properly formatted matrix 
  # created using makeCacheMatrix function to get the inverse of a
  # matrix from cache if exists
  # else creates inverse and stores in cach
  

cacheSolve <- function(x=matrix(), ...) {
    ##get Matrix inverse
    maT <- x$getMatInverse()
    if(!is.null(maT)) {
      message("Input provided is null")
      return(maT)
    }
    else
    {
      ##check if input is a matrix
      # only validating if input is matrix
      # not creating matrix as not specified in instructions
      if(is.matrix(x))
      {
        matrix<- x$get()
        maT<-solve(matrix, ...)
        x$setMatInverse(maT)
        return(maT) ## Return a matrix that is the inverse of 'x'
      }
      else
      {
        message("Input provided is not valid matrix")
        return(maT)
      }
      
    }
}
