
##Make a special square matrix with cache support
makeCacheMatrix <- function(x = matrix()) {
  r <- NULL
  
  set <- function(y) {
    x <<- y
    r <<- NULL
  }
  
  get <- function() x
  
  setResults <- function(solve) {
    r <<- solve
  }
  
  getResults <- function(solve) r
  
  list(set = set, get = get, 
       setResults = setResults, 
       getResults = getResults)
}


##solve or inverse square matrix with cache support
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  r <- x$getResults()
  if(!is.null(r)) {
    message("getting cached data")
    return(r)
  }
  data <- x$get()
  r <- solve(data, ...)
  x$setResults(r)
  r
}



##Testing
A <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)

##Normal Testing
message("Testing Small Matrix...")
message("Input Matrix: ")
A

message("Testing with cache")
x1 <- makeCacheMatrix(A)
x2 <- cacheSolve(x1)
x2

message("Testing without cache")
x3 <- solve(A)
x3

message("Results are identical: ")
identical(x2, x3)


##BIG MATRIX Testing
message("Testing Large Matrix...")
B <- matrix(rnorm(2500), nrow=50, ncol=50)
message("Input Matrix: ")
B

message("Testing with cache")
x1 <- makeCacheMatrix(B)
x2 <- cacheSolve(x1)
x2

message("Testing without cache")
x3 <- solve(B)
x3

message("Results are identical: ")
identical(x2, x3)





