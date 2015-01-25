## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {     ## create a special matrix named x
  s <- NULL                                     ## create the cache
  set <- function(b){                           ## set the dataset of the matrix x
    x <<- b
    s <<- NULL
  }
  get <- function() x                           ## get the dataset of the matrix x
  setsolve <- function(solve) s <<- solve       ## set the value of the inverse of the matrix x
  getsolve <- function() s                      ## get the value of the inverse of the matrix x from the cache
  list(set = set, get = get, 
       setsolve = setsolve, 
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()               ## get the value of the cache              
  if(!is.null(s)){                ## check to see if the inverse of the matrix has already been calculated or exist in the cache
    message("getting cached data")
    return(s)                     ## get the inverse of the matrix from the cache and skip computation
  }
  dat <- x$get()                 ## get the dataset of the matrix x and assign it to dat
  s <- solve(dat,...)            ## calculate the inverse of the matrice x and assign it to s
  x$setsolve(s)                  ## set the value of the inverse of the matrix in the cache
  s
}
