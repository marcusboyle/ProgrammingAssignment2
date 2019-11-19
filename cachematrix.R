## makeCacheMatrix() takes a matrix as input, and provides us with a
## list of functions to "get" and "set" the values for this matrix
## and its inverse. This list is then passed to cacheSolve(), which
## either (a) calculates the matrix inverse, caches and returns it,
## or (b) simply returns the cached value if it exists.

## makeCacheMatrix(): Takes a matrix as input, then creates 4
## functions: getter+setter for the matrix & getter+setter for the
## inverse cache. Then returns these 4 functions in a list.
makeCacheMatrix <- function(x = matrix()) {
      # Initialise cache to null
      inv_cache <- NULL

      ## Matrix functions
      # Setter: takes a matrix and stores it in "x", resets cache
      set_mat <- function(mat) {
            x <<- mat  # "<<-" modifies the variable in the parent function
            inv_cache <<- NULL
      }
      # Getter: returns the matrix "x"
      get_mat <- function() x

      ## Inverse functions
      # Setter: takes an answer for inverse and stores it in "inv_cache"
      set_inv <- function(inv) inv_cache <<- inv
      # Getter: returns the inverse, which was stored in "inv_cache"
      get_inv <- function() inv_cache

      # Return getters/setters inside a length-4 list
      list(set_mat=set_mat, get_mat=get_mat,
           set_inv=set_inv, get_inv=get_inv)
}

## cacheSolve(): Takes the function "makeCacheMatrix()" as input
## (giving us getter/setter functions for the matrix and its inverse),
## -- plus, optionally, additional arguments for "solve()".
## If the inverse of the matrix passed into makeCacheMatrix() has
## been cached, then load the cache -- otherwise get the matrix and
## calculate its inverse, which will be cached and returned.
cacheSolve <- function(x, ...) {
      # Get the cache of the matrix inverse - return it if it isn't null
      inv_cache <- x$get_inv()
      if(!is.null(inv_cache)) {
            message("Loading inverse from cache")
            return(inv_cache)
      }
      # If the cache is null, then we get and invert the matrix
      # that was passed into "makeCacheMatrix()"
      mat <- x$get_mat()
      inv <- solve(mat, ...)
      # Store the inverse in the cache and return it
      x$set_inv(inv)
      inv
}
