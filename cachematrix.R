## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# set: takes variable, sets x=variable, and m=null.
# get: returns x.
# setmean: sets m=mean.
# getmean: returns m.
# makeVector: returns list of 4 functions above.
makeCacheMatrix <- function(x = matrix()) {
    # Initialise cache to null
    inv_cache <- NULL

    ## Matrix
    # Setter: takes a matrix and stores it in "x", resets cache
    set_mat <- function(mat) {
        x <<- mat
        inv_cache <<- NULL
    }
    # Getter: returns the matrix "x"
    get_mat <- function() x

    ## Inverse
    # Setter: takes an answer for inverse and stores it in "inv_cache"
    set_inv <- function(inv) inv_cache <<- inv
    # Getter: returns the inverse, which was stored in "inv_cache"
    get_inv <- function() inv_cache

    # Return getters/setters inside a length-4 list
    list(set_mat=set_mat, get_mat=get_mat,
         set_inv=set_inv, get_inv=get_inv)
}

## Write a short comment describing this function

# pass in: x (list from makeVector)
# m = the m that was set earlier (or is still NULL)
# if m != NULL: restore from cache and return m.
# else, get the numeric vector.
# store the mean of the vector in m.
# use set_mean(m) to store the mean.
# return m.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Check the cache - if it is not null, then return this
    inv_cache <- x$get_inv()
    if(!is.null(inv_cache)) {
        message("Loading inverse from cache")
        return(inv_cache)
    }
    # If the cache is null, then we get and solve the matrix
    # that was set inside list "x"
    mat <- x$get_mat()
    inv <- solve(mat, ...)
    # Store the inverse in the cache and return it
    x$set_inv(inv)
    inv
}
