## Using caching mechanism to only compute the inverse of a matrix once,
## instead of multiple times

## Function that creates a special vector which set and get "methods" for the matrix itself and 
## the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  # the inverse is not defined at the beginning
  inverse <- NULL
  # setter for the matrix themself
  set <- function(y) {
    x <<- y
    # if we change the matrix, we have to reset the inverse to NULL
    # so that it will be recalculated the next time we try to use it
    m <-- NULL
  }
  get <- function() x
  # setter and getter for the inverse
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  # the list containing all the function
  # this is, what is really returned when we call it
  list( set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function computes the inverse of a matrix as stores it
## So the next time, that the function is called
## we can used the cached version instead of computing it everytime
## (as long as matrix didn't change)

cacheSolve <- function(x, ...) {
  # tries to get the inverse
  inverse = x$getinverse()
  # let's check whether we have a chached version
  if(!is.null(inverse)) {
    message("Using chached data...")
    return(inverse)
  }
  # we don't have any cached data, so we have to compute it 
  # get the data for the computation
  data <- x$get()
  # compute it
  inverse <- solve(data)
  # store it for the next time
  x$setinverse(inverse)
  # return it
  inverse
}


# # test cases found @ https://github.com/dayu321/:
# a <-matrix(c(0, 1, 3, 3, -1, -1, 1, 1, 2), 3, 3)
# b <- makeCacheMatrix(a)
# print(cacheSolve(b))
# #      [,1] [,2] [,3]
# # [1,] -0.2 -1.4  0.8
# # [2,]  0.2 -0.6  0.2
# # [3,]  0.4  1.8 -0.6
# print(cacheSolve(b))
# # getting cached data
# #      [,1] [,2] [,3]
# # [1,] -0.2 -1.4  0.8
# # [2,]  0.2 -0.6  0.2
# # [3,]  0.4  1.8 -0.6
