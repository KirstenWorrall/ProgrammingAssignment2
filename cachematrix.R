
# A matrix is created in the makeCacheMatrix function, this matrix will for now always be inversible.
#The function sets the matrix and also gets the values of said matrix, the function then sets and get the value of the inverse. 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
    get <- function() x
    setmatrix <- function(inverse) m <<- inverse
    getmatrix <- function() m
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

# this function returns the inverse of the above makeCacheMatrix function. If the inverse is already calculated, it just gives the original matrix.
cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}

getwd()
