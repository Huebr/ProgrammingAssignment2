## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## create a caching matrix using superassignment and lexical scoping
makeCacheMatrix <- function(x = matrix()) {
      ##internal caching value
      m<-NULL 
      ## set x and m in parent environment set a new value to x and signal m state to clean state with NULL value
      set <- function(y){
         x <<- y
         m <<- NULL
      }
      get <- function() x
      setmatrix <- function(m_matrix) m<<- m_matrix
      getmatrix <- function() m
      list(set=set,get=get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
}


## Write a short comment describing this function
## return inverse of x if the value has already been computed returns cached value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)){
           message("getting cached data")
           return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setmatrix(m)
        m
}
