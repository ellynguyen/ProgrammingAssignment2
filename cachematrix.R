## The following functions create a particular matrix and calculate its inverse matrix
  #with solve function, these values are also cached in a different environment. 
  # If the matrix has not been changed, its inverse cache matrix is used instead of
  # calculating a respective inverse matrix for it.

## makeCacheMatrix do the following tasks
  # create a matrix (set)
  # Get/Return a matrix (get)
  # Set the inverse matrix (set_inverse_matrix)
  # Get/Return the inverse matrix (get_inverse_matrix)

makeCacheMatrix <- function (x = matrix()) {
  cache_invert_mt <- NULL
  
  set <- function (mt) {
    x <<- mt
    cache_invert_mt <<- NULL
  }
  
  get <- function() x
  
  set_invert_matrix <- function (mt){
    cache_invert_mt <<- mt
  }
  
  get_invert_matrix <- function () cache_invert_mt
  
  list(set = set, get = get, set_invert_matrix = set_invert_matrix, get_invert_matrix = get_invert_matrix)
}


## cacheSolve function gets the inverse matrix of a respective matrix from cache
  # if the inverse cache matrix has not existed in the cache, get the matrix and
  # calculate its inverse matrix then cache the later to the cache

cacheSolve <- function(x, ...) {  
  ## Return a matrix that is the inverse of 'x'
  invert_mt <- x$get_invert_matrix ()
  
  if(!is.null(invert_mt)){
    message("getting cached data")
    return (invert_mt)
  }
  
  mt <- x$get()
  invert_mt <- solve(mt)
  x$set_invert_matrix(invert_mt)
  
  invert_mt  
}
