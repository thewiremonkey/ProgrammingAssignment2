## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
        
        #assigns the cacheSolve inv dataset after cacheSolve has run
        
        setinv<-function(inverse) inv<<-inverse 
        
        #load the inv dataset after it has been created.
        getinv<-function() inv
        
        #the list of subfunctions available within the parent function
  
        list(get=get, set=set, setinv=setinv, getinv=getinv)
 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if(!is.null(inv)){ ##checks to see if the variable inv is stored in the Global Environment
                
                #message("getting cached data") #alerts the user that the cached data is being retrieved
                #inv<-x$getinv()
        
        }
        else
                
                {
                        message("hold on while I calculate the inverse")
                        data<-x$get()  #as long as the data has been cached, 
                        inv<-solve(data)
                        x$setinv(inv)
                        
                }
        
       inv
}
