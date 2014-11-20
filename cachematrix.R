## This R script defines a special object of our own creation, which is not 
## an atomic data type in R. The object has two internal attributes. The
## first 'x' is a matrix and the second 'inv' is the inverse of that matrix.
## This inverse matrix is stored in the object to save computation time
## if it is required later as it simply has to be returned.

## This is the constructor function called when we wish to create a new
## special 'matrix' object. If no matrix is provided, then an empty one is
## created by default.
## The constructor also defines the getter & setter functions which can
## execute on the special 'matrix' object.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(mat){
    x <<- mat
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(i) inv <<- i 
  
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function takes a special 'matrix' object as an input and 
## will retutn it's inverse in one of two ways.
## If the inverse has already been computed and is stored in the cache
## then the value is simply returned. If the inverse has not been computed
## then the function will compute it, store it in the cache and return
## the solution.
## I have used two different variables 'inv' and 'i_mat' just to make it
## clear where the values have come from. 'inv' is the value found in the
## cache and 'i_mat' is the inverse which we have computed here.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  
  ## check to see if inverse has already been calculated
  ## if so, using return to break from code and return solution
  if(!is.null(inv)){
    message("returning cached inverse matrix")
    return(inv)
  }
  
  ## else get the original matrix & calculate the inverse
  o_mat <- x$get()
  i_mat <- solve(o_mat, ...)
  ## now set the value of inv from our calculation
  x$setinv(i_mat)
  ## then return the solution
  i_mat ## although this could be return(x$getinv())
}
