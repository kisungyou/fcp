# auxiliary functions -----------------------------------------------------
#   (1) dynprog_main : do dynamic programming
#   (2) dynprog_nfcp : cost function for the framework


# (1) dynprog_main --------------------------------------------------------
#' @keywords internal
dynprog_main <- function(listfcp, Kmax=min(10,length(listfcp)), Lmin=1, gamma=0, print.progress=TRUE){
  # prepare
  Nr = round(Kmax - 1)
  n  = length(listfcp)
  V  = array(Inf, c(n,n))
  
  # compute the cost
  totaliter = 0
  for (j1 in 1:(n-Lmin+1)){
    for (j2 in ((j1+Lmin-1):n)){
      totaliter = totaliter + 1
    }
  }
  iter = 0
  for (j1 in 1:(n-Lmin+1)){
    for (j2 in ((j1+Lmin-1):n)){
      partfcp  = listfcp[j1:j2]
      V[j1,j2] = dynprog_nfcp(partfcp) + gamma # cost + penalty / maybe add later
      iter = iter + 1
      if (print.progress){
        print(paste0("* nfcp : iteration for kernel ridge regression ",iter,"/",totaliter," complete..."))  
      }
    }
  }
  
  # compute others
  U = rep(0,Kmax)
  U[1] = V[1,n]
  D = V[,n]
  Pos <- matrix(nrow = n, ncol = Nr)
  Pos[n,] <- rep(n,Nr)
  tau.mat <- matrix(nrow = Nr,ncol = Nr)
  for (k in 1:Nr){
    for (j in 1:(n-1)){
      dist <- V[j,j:(n-1)] + D[(j+1):n]
      D[j] <- min(dist)
      Pos[j,1] <- which.min(dist) + j
      if (k > 1) { Pos[j,2:k] <- Pos[Pos[j,1],1:(k-1)] }
    }
    U[k+1] <- D[1]
    tau.mat[k,1:k] <- Pos[1,1:k]-1
  }
  ## this part for adding (|P|-1)*gamma
  U = U - gamma
  out <- list(Test=tau.mat, obj=data.frame(K=((1:Kmax)),U=U))
  return(out)
}

# (2) dynprog_nfcp  -------------------------------------------------------
#' @keywords internal
dynprog_nfcp <- function(listpart){
  N = length(listpart)
  
  # train a model
  train.t = c()
  train.f = c()
  for (n in 1:N){
    train.t = c(train.t, listpart[[n]]$t)
    train.f = c(train.f, listpart[[n]]$f)
  }
  nfcpobj = listdtr::krr(train.t, train.f)
  
  # learning over each object
  output = 0
  for (n in 1:N){
    obj.t  = listpart[[n]]$t
    f.data = listpart[[n]]$f
    f.pred = predict(nfcpobj, xnew=obj.t)
    output = output + sum((f.data-f.pred)^2)
  }
  
  # return
  return(output)
}


# dynProg.mean <- function(y, Kmax, Lmin=1, lambda=0) 
# {
#   Nr  <- Kmax - 1
#   n <- length(y)
#   V <- matrix(Inf, nrow = n, ncol = n)
#   for (j1 in (1:(n-Lmin+1))){
#     for (j2 in ((j1+Lmin-1):n)) {
#       yj <- y[j1:j2]
#       nj <- j2-j1+1
#       V[j1,j2] <- cost.mean(yj) + lambda
#       # V[j1,j2] <- compute.sse(yj) # fitting linear model
#       # V[j1,j2] <- sum(yj^2) - (sum(yj)^2)/nj + lambda# == sum((yj-mean(yj))^2); non-normalized error
#     }
#   }
#   
#   U <- vector(length=Kmax)
#   U[1] <- V[1,n]
#   D <- V[,n] 
#   Pos <- matrix(nrow = n, ncol = Nr) 
#   Pos[n,] <- rep(n,Nr)    
#   tau.mat <- matrix(nrow = Nr,ncol = Nr) 
#   for (k in 1:Nr){
#     for (j in 1:(n-1)){
#       dist <- V[j,j:(n-1)] + D[(j+1):n]
#       D[j] <- min(dist)
#       Pos[j,1] <- which.min(dist) + j
#       if (k > 1) { Pos[j,2:k] <- Pos[Pos[j,1],1:(k-1)] }
#     }
#     U[k+1] <- D[1]
#     tau.mat[k,1:k] <- Pos[1,1:k]-1
#   }
#   out <- list(Test=tau.mat, obj=data.frame(K=((1:Kmax)),U=U))
#   return(out)
# }