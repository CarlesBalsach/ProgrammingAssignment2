## This function work very much as a class in any Object Oriented Programming language
## where it stores some values inside it (instead I guess it's shared)
## The values stored will be the matrix itself and the inverse of that matrix (with lazy evaluation),
## meaning that it will be computed once and store it's value for further queries of it

## This function acts as the class itslef

makeCacheMatrix <- function(x = matrix())
{
    m <- NULL  #This is the cached value of the ivnerse matrix of X
    set <- function(y) { #This function sets X matrix, and removes the previous value of it's inverse (since it changed the inverse in gonna be different as well)
        x <<- y
        m <<- NULL
    }
    get <- function() x #A standard getter method for the matrix X
    setinverse <- function(inverse) m <<- inverse #This stores the inverse value in variable m, in a more general scope
    getinverse <- function() m # A standard getter method for the inverse of matrix X
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) # We define here the getters and setters methods (naming them the same as the name of our functions, optional though)
}


## This function will evaluate and return the inverse of a given matrix X, with the particularity of storing it's value when computed, making it (almost) instant for further queries of it

cacheSolve <- function(x, ...) #this takes a makeCacheMatrix fcuntion as parameter [x]
{
    m <- x$getinverse() #getting the cached value
    if(!is.null(m)) { #if value exists...
        message("getting cached data")
        return(m) #we return it
    } #otherwise we compute it before returning it
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    invisible(m)
}

# This "class" or set of functions are ment to be used like this

# m <- matrix(c(4,2,7,6),2,2) # This sets our matrix
# cm <- makeCacheMatrix(m) # This is the object of our CachedMatrix
# im <- cacheSolve(cm) # This will calculate and give us the inverse matrix, caching it internally on the cm class
# im2 <- cacheSolve(cm) # Now it will return its catched value, printing the message "getting cached data"

# We can see that identical(im,im2) prints TRUE

# We can also change the matrix without creating a new object like this:

# cm$set(matrix(c(1,2,3,4),2,2)) # If we run cm$get we'll see that we have a matrix consisting of integers 1,2,3,4
# im <- cacheSolve(cm) # Now the "getting cached data" does not promt, since we did reset the cached value (it will calculate and store it again now)
# im <- cacheSolve(cm) # Now we'll see the message, since it's been calculated before.

# Hope this was clear enough!
# BEST!
# Carles
