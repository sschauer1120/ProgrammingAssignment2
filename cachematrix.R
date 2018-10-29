## makeCacheMatrix is a function that takes a matrix, computes the inverse of that 
## matrix using the function solve().  Because computation of the inverse matrix
## takes a degree of computing power, it is stored in a cache outside the immediate
## environment to eliminate the need to compute it each time r needs to loop over the
## code

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL                                  # initialize i
        set <- function(y) {                       # set the value of the matrix 
                x <<- y                            # assign x = y, specifying with <<- that r can search for the values outside the immediate environment 
                i <<- NULL                         # initialize i within the function specifying with <<- that r can search for i outside the immediate environment          
        }
        get <- function() x                        # a function with no formal arguments that gets the value of the inputed matrix 'x'  
        setInv <- function(solve) i <<- solve      # a function that computes the inverse matrix using the solve() function, using i, the set value of the matrix x 
        getInv <- function() i                     # a function with no formal arguments that gets the value of the inverse matrix 'i' 
        list(set = set,                            # create a list of 4 elements consisting of the objects assigned above 
             get = get,
             setInv = setInv,
             getInv = getInv)
}

## cacheSolve takes the special matrix object created by makeCacheMatrix
## and checks to see if inverse matrix value already exists in the cache
## if not, it calculates it and returns the value 

cacheSolve <- function(x, ...) {                   
        i <- x$getInv()                            # set i = the value of the inverted matrix computed from the matrix 'x'
        if(!is.null(i)) {                          # if i already exists 
                message("getting cached data")     # message on status
                return(i)                          # r returns the cached value in i stored outside the immediate environment
        }
        data <- x$get()                            # otherwise, r gets the matrix value from x stored in the get function and assigns it to 'data'
        i <- solve(data, ...)                      # calculates inverse of that matrix value from 'data' and assigns it as the object 'i'
        x$setInv(i)                                # sets object 'i' as the inverse matrix of x  
        i                                          # returns the inverse matrix stored as i
}

## Output of the code above:
##
## >x <- matrix(c(1, 2, 3, 4), 2, 2)
## >x
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## >CACHE <- makeCacheMatrix(x)
## >CACHE
## $set
## function (y) 
## {
##         x <<- y
##         i <<- NULL
## }
## <environment: 0x103dd5210>
##        
##        $get
## function () 
##         x
## <environment: 0x103dd5210>
##         
##         $setInv
## function (solve) 
##         i <<- solve
## <environment: 0x103dd5210>
##        
##         $getInv
## function () 
##         i
## <environment: 0x103dd5210>
##
## > cacheSolve(CACHE)
##       [,1] [,2]
## [1,]    -2  1.5
## [2,]     1 -0.5
