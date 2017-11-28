# make a iteration function for numerical solving method

fixedpoint <- function(ftn,x0,tol=1e-9,max.iter=100){
    #first iteration
    xold=x0
    xnew=ftn(xold)
    iter<-1
    cat("At iteration 1 value of x is:",xnew,"\n")
    # continue iteration
    while ((abs(xnew-xold)>tol) && (iter < max.iter)){
        xold<-xnew;
        xnew <- ftn(xold);
        iter<- iter+1
        cat("At iteration",iter,"value of x is:",xnew,"\n")
    }
    #output depends on success of algorithm
    if (abs(xnew-xold)>tol){
        cat("Algorithm failed to converge\n")
        return(NULL)
    } else {
        cat("Algorithm converge\n")
        return(xnew)
    }
}

# e.g. finding root of f(x) = log(x)-exp(-x)

ftn1 <- function(x) return(exp(exp(-x)))
fixedpoint(ftn1,2,tol=1e-6)


# newton method
newtonraphson <- function(ftn,x0,tol=1e-9,max.iter=100){
    x <- x0
    fx <- ftn(x)
    iter <- 0
    
    while((abs(fx[1]) > tol) && (iter<max.iter)){
        x<- x-fx[1]/fx[2]
        fx <- ftn(x)
        iter<iter+1
        cat("At iteration",iter,"value of x is:",x,"\n")
    }
    if (abs(fx[1])>tol){
        cat("Algorithm failed to converge\n")
        return(NULL)
    } else {
            cat("algorithm converge\n")
        return(x)
        }
}

ftn4 <- function(x){
    fx <- log(x)-exp(-x)
    dfx <- 1/x + exp(-x)
    return(c(fx,dfx))
}

newtonraphson(ftn4,2,1e-6)
