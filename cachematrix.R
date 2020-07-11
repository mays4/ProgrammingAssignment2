
##create a function special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y){
    x<<- y 
    n <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) n <<- inverse 
  getInverse  <- function() n
  list(set = set, get = get,
       setInverse  = setInverse ,
       getInverse  = getInverse )
  
}

## calculate the inverse for th above function 
cacheSolve <- function(x, ...) {
  n <- x$getInverse()
  if(!is.null(n)){
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data,...)
  x$setInverse(n)
  n
  
}