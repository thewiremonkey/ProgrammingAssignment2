## Put comments here that give an overall description of what your
## functions do:

##These functions can calculate and store the inverse of a matrix in the cache
##so that the script does not need to recalculate it every time it is needed.  

## The first function creates a cache from a data matrix and, if it has been calculated with cacheSolve (or 
## if it has been independently calculated), the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv=NULL  #initialize the inverse function data vector to null
        
        
        #if you have placed a dataset in the parent function or via the $set function,
        #returns the dataset
        
        get<-function() x 
        
        #sets the data for the function and clears the variable inv so you
        ##can start with an empty inv variable
        
        set<-function(y) {  
                x<<-y  #sets the data to the new dataset
                inv<<-NULL
        }
        
        #assigns a value to either a loaded inverse matrix or the results of cacheSolve
        
        setinv<-function(inverse) inv<<-inverse 
        
        #load the inv dataset after it has been created.
        getinv<-function() inv
        
        #the list of subfunctions available within the parent function
  
        list(get=get, set=set, setinv=setinv, getinv=getinv)
 
}


## First checks to see if the inv variable has been cached already.  If so, it loads that matrix directly, otherwise
##it calculates the inverse and then stores it for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv() #pulls the value of inv from the getinv function in makeCacheMatrix
        if(!is.null(inv)){ ##checks to see if the variable inv is null.  If it is not null, it loads the already-calculated
        ##matrix.  It provides a message so the user knows that the inv matrix was alreadyt there.
                
                message("lucky you! The inverse matrix is already calculated") #alerts the user that the cached data is being retrieved
               
        }
        else
              ##if the inv variable is null, this function will create the inverse matrix and assign its value
              ##to inv.  It then sends inv back to makeCacheMatrix and stores it via the setinv function so that it
              ##can be used next inv is called.
                {
                        message("hold on while I calculate the inverse")
                        data<-x$get()  #as long as the data has been cached, 
                        inv<-solve(data)
                        x$setinv(inv)
                        
                }
        
       inv
}
