## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix<-function(A=matrix()) {
        inverse<-NULL
        previous<-NULL
        set<-function(X) {
                A<<-X
                inverse<<-NULL
                previous<<-X
        }
        get<-function() A
        setInverse<-function(inv) inverse<<-inv
        getInverse<-function() inverse
        hasChanged<-function(B) {
                if (B==previous)
                        flag<-TRUE
                else
                        flag<-FALSE
                flag
        }
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse, hasChanged=hasChanged)
}


## Write a short comment describing this function

cacheSolve<-function(A, ...) {
        inverse<-A$getInverse()
        flag<-A$hasChanged(A$get())
        if (!is.null(inverse) && flag==TRUE) {
                message("getting cached data")
                return(inverse)
        }
        mat<-A$get()
        inverse<-solve(mat, ...)
        A$setInverse(inverse)
        inverse
}
