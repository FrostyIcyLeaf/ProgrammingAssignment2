## As with the cached vector example there are two functions defined.
## The first function makeCacheMatrix is the function allowing to cache
## the inverse of a matrix. The second function allows to calculate the
## inverse of a matrix and will return the cached result if the inverse
## has previously already calculated.

## makeCacheMatrix is a customized function describing a matrix. In addition
## to the matrix it will allow the storage of the inverse of the matrix.
## It has four sub functions to set and get the input matrix and to
## set and get the inverse of the input matrix. All the information is stored
## in a list.

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function()
        x
    setiv <- function(i)
        inverseMatrix <<- i
    getiv <- function()
        inverseMatrix
    list(
        set = set,
        get = get,
        setiv = setiv,
        getiv = getiv
    )
}

## cacheSolve is a function that calculates the inverse of a given matrix
## that has been defined via makeCahceMatrix.
## The function will first atempt to get a previously calculcated, cached
## inverse matrix. If an inverse matrix is cached it will return the cached
## result. Otherwise, it will take the input matrix, calculate the inverse
## set the inverse matrix for future retrievals and return the inverse.

cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getiv()
    if (!is.null(inverseMatrix)) {
        message("getting cached inverse matrix")
        return(inverseMatrix)
    }
    data <- x$get()
    inverseMatrix <- solve(data)
    x$setiv(inverseMatrix)
    inverseMatrix
}