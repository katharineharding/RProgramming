## Submission for R programming course, programming assignment 2
## 22nd March 2015

## This function essentially sets up a space where all the relevant information can be stored 
## as a cache, and then accessed by the second function
##
## the function takes a matrix as its input
##
## s is set as the inverse of the matrix, and initially set to NULL, so that the first time
## the code runs s has not yet been calculated.  Subsequently, it can be calculated and then
## saved for later use
##
## the function returns a list of the environments where each value has been saved

makeMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(solve) s <<- solve
  getinv <- function() s
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function now calculates the inverse of the matrix.
## It looks to see whether the inverse has already been calculated and if so it prints that out
## If not (the first time that the function is run) then it will calculate the inverse and print that
## It takes the output of the makeMatrix function as its data input
## It outputs the inverse of the matrix

cacheMatrix <- function(x, ...) {
  s <- x$getinv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}

## I've tested this using random matrices (2 by 2, & also 3 by 3) and it has worked.
## Thank you very much for reading.