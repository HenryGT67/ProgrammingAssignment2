## I have two functions makeCacheMatrix() and cacheSolve(). The first function, makeCacheMatrix (),
## creates an R object that store a matrix (we assume that it is invertible) and its inverse.
##The second function cacheSolve() requires an argument that is returned by makeCacheMatrix() 
##in order to retrieve the inverse from the cached value that is stored in the makeCacheMatrix()
##object's environment.

## the function makeCacheMatrix builds a set of functions (set(),get(),setsolve(),getsolve())
##and two data objects (x,inv) and returns the functions within a list to the parent environment.

makeCacheMatrix <- function(x = matrix()) {
                 inv<-NULL         #Assigns the value of NULL to the 'inv' object in the parent environment
                                   #so the 'inv' object is cleared when 'x' object has a new input
                 set<-function(y){ #defines the function set(), program module that assign the 
                                   #input argument to the 'x' object in the parent environment
                         x<<-y
                         inv<-NULL   #Assigns the value of NULL to the 'inv' object in the parent environment
                                     #so the 'inv' object is cleared whenever 'x' is reset
                         }
                  get<-function()x  #defines the function get(), that is the getter for the 'x' object
                                    #'x' is not defined within get() so R takes this value from the parent environment
                  setsolve<-function(solve)inv<<-solve #defines the function setsolve() that is the setter for
                                                       #the inverse of the matrix 'inv' in the parent environment
                  getsolve<-function()inv  #defines the getter for the inverse 'inv' so R retrieves this value 
                                           #from the parent environment
                  ##Assigns each of these functions as an element within a list and returns it
                  ##to the parent environment. Each element in the list is named.
                  list(set=set # gives the name 'set' to the set() function defined above
                       get=get #gives the name 'get' to the get() function defined above
                       setsolve=setsolve #gives the name 'setsolve' to the setsolve() function defined above
                       getsolve=getsolve) #gives the name 'getsolve' to the getsolve() function defined above
}


## The function cacheSolve() calculates and stores the inverse for the input argument, this 
## argument of type list like the list of data from makeCacheMatrix output.

cacheSolve <- function(x, ...) {
           inv<-x$getsolve() #Assigns the value of the element getsolve from the list in makeCacheMatrix
           if(!is.null(inv){
                   message("getting cached data") #Check the cached to avoid recalculate the same
                                                  #stored object in the parent environment
                   return(inv)
                   }
              data<-x$get() 
              inv<-solve(data,...)
              x$setsolve(inv)
              inv          #gives the inverse value of the argument 
}
