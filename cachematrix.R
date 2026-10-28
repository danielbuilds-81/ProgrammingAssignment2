
#==============================================================================#
#         Programming Assignment 2: aching the Inverse of a Matrix.            #
#==============================================================================#
# The goal is to avoid repeated, costly matrix inversion calculations by caching 
# the inverse of a matrix. If the matrix changes, the cached inverse is reset.
#

# Function: makeCacheMatrix Creates an object that stores a matrix and its 
# inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
                
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
                
        get <- function() {
                x
        }
        
        setinverse <- function(inverse) {
                m <<- inverse
        }
                
        getinverse <- function() {
                m
        }
                
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}



# Function: cacheSolve Returns the cached inverse if available; otherwise, 
# calculates the inverse using solve() and caches the result.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        
        if (!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
                
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
                
        m
}


# Testing 
mat <- makeCacheMatrix(
        matrix(c(1, 2, 3, 4), nrow = 2)
)

# Calculate the inverse and cache the result
cacheSolve(mat)

#Retrieve the cached inverse
cacheSolve(mat)







