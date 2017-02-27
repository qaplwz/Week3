## A pair of functions that cache the inverse of a matrix


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        ## Initialize the inverse property
        inverse<-NULL
        ## set the matrix
        set<-function(y){
                x<<-y;
                inverse<<-NULL;
                }
        ## get the matrix
        get <- function() {
        x
        }
        ## set the inverse of the matrix
        setinv<-function(inv){
        inverse<<-inv
        }
        ## get the inverse of the matrix
        getinv<- function() {
        inverse
        }
        ##return a list of the method
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}



## compute the inverse of the psecial matrix returned by "makeCacheMatrix"

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

        inverse<-x$getinverse()

        ## reutrn the inverse if its already set

        if(!is.null(inverse)){
            message("getting cached data")
            return(inverse)
        }

        ##get the matrix from the object
        data<-x$get()

        ##calculate the inverse using matrix multiplication
        inverse<-solve(data,...)

        ##set the inverse to the object
        x$setinverse(inverse)

        ##return the martix
        inverse
}

