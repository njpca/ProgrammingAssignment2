#Week 3 Programming Assignment: Caching the Inverse of a Matrix

##makeCacheMatrix and cacheSolve collectively cache and retrieve solved
##matrices in order to avoid processor-intensive, tedious, and duplicative
##matrix solving for matrices used repeatedly.

##makeCacheMatrix defines the "set" and "get" (or "mutator" and "accessor")
##functions which define and retrive stored data for cacheSolve. makeCacheMatrix
##Takes as its argument a matrix; its output is used as the primary argument
##for cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function()x
        setinv<-function(solve) inv<<-solve
        getinv<-function() inv
        list(
                set=set,
                get=get,
                setinv=setinv,
                getinv=getinv)
}


##cacheSolve utilizes the subsidiary functions within makeCacheMatrix to test
##whether a matrix's solution has already been calculated, and, if so, returns
##the previously cached solution (and a message stating as much); if not,
##cacheSolve calculates, caches, and returns the solution for said matrix.

cacheSolve<-function(x,...){
        inv<-x$getinv()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinv(inv)
        return(inv)
}
