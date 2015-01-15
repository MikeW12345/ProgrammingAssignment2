## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Function makeCacheMatrix creates a "matrix" object, calculates inverse 
## (using solve) and caches result

makeCacheMatrix <- function(x = matrix()) {

inv <- NULL                    ## sets matrix inverse value to NULL
  # print(environment())       Test line used during development
evn <- environment()  
  # print(parent.env(evn))     Test line used during development
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
          }
  
  get <- function() x
  setinv <- function(solve) inv <<- solve(x)  ## sets value of inverse in object "inv"
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function

## Function cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), the function 
## retrieves the inverse from the cache with the message "using cached version of inverse".

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
  if(!is.null(inv)) {                              ## tests if inverse exists in cache (valus not null)
    message("using cached version of inverse")
    return(inv)                                    ## returns cached inverse if inverse already present
  }                                                ## else (if no cached version exists)
  data <- x$get()
  inv <- solve(data, ...)                          ## inverse is calculated and returned
  x$setinv(inv)
  inv
}
