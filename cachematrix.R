## Two functions to cache the matrix inverse...

## In this function, a matrix object is created which will cache the inverse of that matrix...

makeCacheMatrix <- function(x = matrix()) 
{
  inverse <- NULL
  set <- function(y)
    {
      x <<- y
      inverse <<- NULL
    }
  get <- function() x
  set_inverse <- function(compute_matrix)inverse<<- compute_matrix
  get_inverse <- function()inverse
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## In this function, the inverse of the matrix returned by the above function "makeCacheMatrix" is computed...

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
    inverse <- x$get_inverse()
    if(!is.null(inverse))
      {
         message("getting the priorly cached data ....")
         return(inverse)
      }
    new_data <- x$get()
    inverse <- solve(new_data)
    x$set_inverse(inverse)
    inverse      
}