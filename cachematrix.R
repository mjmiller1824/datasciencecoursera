## makeCacheMatrix takes a matrix from the global environment stores it in a new local variable called x. 
makeCacheMatrix <- function(x = matrix()) {#initializes function makeMatrixCache
  
  m <- NULL #sets local variable m to NULL
  set <- function(y) {#initialize function set. Set allows user to set matrix if they forgot to pass an arguement in the initial function call.
    x <<- y #sets variable x in parent environment to argument passed from function set call
    m <<- NULL #sets global variable m to NULL ensuring that it will not be full when you call cacheSolve
  }
  get <- function() x #initialize function get function returns stored matrix before inversion
  setmatrix <- function(solve) m <<- solve #initializes setmatrix by passing an inverted matrix(via solve) to the global variable m
  getmatrix <- function() m #returns  variable m in parent frame
  list(set = set, get = get,#list of functions within the makeCacheMatrix environment
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## cacheSolve checks to see if m has data in it. If it does it returns the data. If it doesn't it computes the inversion with solve

cacheSolve <- function(x, ...) {#initializes cacheSolve and can take a makeMatrix object as its argument.
  m <- x$getmatrix()#sets local variable m to the value returned by getmatrix(). If getmatrix()is called before cacheSolve, NULL should be returned
  if(!is.null(m)) {#checks to see if local varible m has data. Returns logical value. If m has an inverted matrix stored in it, it returns the inverted matrix
    message("getting cached data")
    return(m)
  }
  data <- x$get()#if the above returns FALSE the value of x is gotten and stored in data. Data is then inverted using the solve function and cached in m
  m <- solve(data, ...)
  x$setmatrix(m)
  m
  
}  