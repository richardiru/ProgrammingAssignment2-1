## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
              # Initially set to NULL
	        # Changes when the user sets the value
        inv <- NULL

             # set function
             # Sets the matrix itself but not the inverse
        set <- function(y) 
         {
                x <<- y
                inv <<- NULL
         }
               # get function
               # Gets the matrix itself but not the inverse
        get <- function() x

               # Manually set the inverse
        setInverse <- function(inverse) inv <<- inverse
           
               # Get the inverse
        getInverse <- function() inv

               # Prepairing into a list
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
##TESTING 

#> source("ProgrammingAssignment2/cachematrix.R")
#> my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
#> my_matrix$get()
#     [,1] [,2]
#[1,]    1    3
[2,]    2    4
#> my_matrix$getInverse()
#NULL
#> cacheSolve(my_matrix)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(my_matrix)
#getting cached data
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> my_matrix$getInverse()
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> my_matrix$set(matrix(c(2, 2, 1, 5), 2, 2))
#> my_matrix$get()
#     [,1] [,2]
#[1,]    2    1
#[2,]    2    5
##> my_matrix$getInverse()
#NULL
#> cacheSolve(my_matrix)
#        [,1]   [,2]
#[1,]  0.625 -0.125
#[2,] -0.250  0.250
#> cacheSolve(my_matrix)
#getting cached data
#       [,1]   [,2]
#[1,]  0.625 -0.125
#[[2,] -0.250  0.250
#> my_matrix$getInverse()
#       [,1]   [,2]
#[1,]  0.625 -0.125
#[2,] -0.250  0.250

## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'

            # Get the current state of the inverse and see if it
            # has been computed yet
        inv <- x$getInverse()

             # If it has 
        if (!is.null(inv)) 
         {
                  # Then return the computed inverse	
                message("getting cached data")
                return(inv)
         }
            # Else ...
            # Get the matrix itself
        mat <- x$get()

            # Find the inverse
        inv <- solve(mat, ...)

            # Cache this result in the object
        x$setInverse(inv)

           # Return this new result
        inv
}

