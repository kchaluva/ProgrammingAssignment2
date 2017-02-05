## Write pair of functions that cache the invers of a matrix.
## 
## Following two functions are used to create a special object to
## store matrix and cache its inverse.
## Caching helps to reduce costly computation like inverse of a matrix
## for already computed inverse of a given matrix in a repeated calls.
##

## makeCacheMatrix: creates a special matrix object that can cache its inverse


makeCacheMatrix<-function(x=matrix())
{
    ## initialize properties
    
    ## initialize the inverse property "inv"
    inv<-NULL
    
    ## set function. 
    ## sets the matrix "x" to the given input "y"
    ## initializes the "inv" property to null
    set<-function(y)
    {
      x<<-y
      inv<<-NULL
    }
    
    # sets inverse property "inv" 
    setinverse<-function(inver) inv<<-inver
    
    ## get function
    ## get function returns the matrix "x"
    
    get<-function() x
    
    ## getinverse function returns the "inv" property which holds
    ## inverse of the matrix x
    getinverse<-function() inv
    
    ## last statement in function
    ## returns list of methods 
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
    
}


## cacheSolve: Computes the inverse of the matrix and caches it.
##             Takes the input as special matrix object returned 
##             by makeCacheMatrix.
##             If the inverse of the matrix is already computed by
##             the previous execution for the given matrix then it
##             should return it from cache without computing inverse
##             again.

cacheSolve<-function(x,...)
{
    ## sets "inv" inverse matrix for the given matrix x
    inv<-x$getinverse()
    
    ## check if the inverse already exists then return it.
    if(!is.null(inv))
    {
      message("getting chached inverse matrix")
      return(inv)
    }
    ## get input given matrix
    inmatrix <-x$get()
    
    ## compute inverse for the input matrix "inmatrix"
    inv<-solve(inmatrix)
    
    ## set the "inv" property of the object
    x$setinverse(inv)
    
    ## return the inverse matrix for "x"
    inv
  
}
