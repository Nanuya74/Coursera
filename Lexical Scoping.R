##I have manually set the working directory to the file my data is in

## what I have done in my function is similar to the example given in the assignment, 
## to create the MakeCacheMatrix function by inputting a matrix called m, 

MakeCacheMatrix <- function (m = matrix ()) {
##assuming the matrix is invertible setting it to NULL, then I created another functioncalled set, that is then set to be the input matrix
  inv <- NULL
  set <- function (s){
    m <<- s
    inv <<- NULL
  }
##later I get the matrix, inverse it, and then cache its inverse and return the results as a list.
  get <- function() {m}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## and the next part is the get Solve function, again it is similar to the example given, but I have 
## have the input matrix as m, and in case of no previous calculatios, calculate the inverse. 

cacheSolve <- function(m, ...){
  inv <- m$getInverse()
  ##checking to see if the inverse has already been calculated
  if (!is.null(inv)){
    message ("Getting the cached data")
    return(inv)
  }
  ##if the inverse not calculated already, it will be calculated here
  mat <- m$get()
  inv <- solve(mat, ...)
  m$setInverse(inv)
  inv
}