## An overall description of what the functions do

## makeCacheMatrix is a function which returns a list 
## containing a function to set, get the matrix 
## and set, get the inverse

## cacheSolve is a function which returns inverse of a matrix
## from cache (if its available) else it calculates the inverse

## Short comment describing this function
## x is square invertible matrix and is not singular
## returns a list of functions

makeCacheMatrix <- function(x = matrix()) 
{

    temp  <-  NULL
        set_mat  <-  function(y) 
		{
                x <<- y
                temp <<- NULL
        }
        get_mat  <-  function() x
        set_inv_mat  <-  function(inv) temp <<- inv 
        get_inv_mat  <-  function() temp
        list(set_mat = set_mat, get_mat = get_mat, set_inv_mat = set_inv_mat, get_inv_mat = get_inv_mat)
}

## Short comment describing this function
## cacheSolve is a function which returns inverse of a matrix
## from cache (if its available) else it calculates the inverse

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
    temp <- x$get_inv_mat()
	## If available in cache then from Cache
        if (!is.null(temp))
		{
                message("retreiving cached matrix from cache")
                return(temp)
        }	
    ## Otherwise
        temp_mat <- x$get_mat()
        temp <- solve(temp_mat, ...)
        x$set_inv_mat(temp)
        
        return(temp)
}

## Sample Output of the Program

## > dir()
## [1] "cachematrix.R" "README.md"    
## > source('cachematrix.R')
## > r <- rnorm(25)
## > mat <-  matrix(r, nrow=5, ncol=5)
## > mat
##            [,1]       [,2]       [,3]        [,4]       [,5]
## [1,]  1.6993779  0.9701787 -0.5678446  0.06421042 -0.3587946
## [2,] -0.8410334  1.0642587  0.8314788 -0.20695874 -0.4448137
## [3,] -0.2484687  0.2024740 -0.6340980 -1.20835166 -0.5774957
## [4,]  0.1632434  0.1662399 -1.6704878  0.95437875  0.5915637
## [5,] -0.8669817 -0.4389312  2.4569573 -0.39961605  0.8675474
## > temp <- makeCacheMatrix(mat)
## > temp2 <- cacheSolve(temp)
## > temp2
##             [,1]       [,2]        [,3]       [,4]        [,5]
## [1,]  0.46353631 -0.3453441 -0.08421099 -0.1789458  0.08060298
## [2,]  0.40700854  0.5693013  0.20580220  0.4719552  0.27540149
## [3,]  0.07575928  0.0793030 -0.30143621 -0.3293821  0.09593650
## [4,] -0.23287038  0.2126048 -0.84836708 -0.1522349 -0.44822312
## [5,]  0.34733570 -0.1837440  0.48287672  0.9226647  0.89440026
## > temp2 <- cacheSolve(temp)
## retreiving cached matrix from cache
## > temp2
##             [,1]       [,2]        [,3]       [,4]        [,5]
## [1,]  0.46353631 -0.3453441 -0.08421099 -0.1789458  0.08060298
## [2,]  0.40700854  0.5693013  0.20580220  0.4719552  0.27540149
## [3,]  0.07575928  0.0793030 -0.30143621 -0.3293821  0.09593650
## [4,] -0.23287038  0.2126048 -0.84836708 -0.1522349 -0.44822312
## [5,]  0.34733570 -0.1837440  0.48287672  0.9226647  0.89440026
## > 