#' Nonparametric Change Point Detection
#' 
#' @export
nfcp <- function(obj.fcp, Kmax=min(5, (length(obj.fcp)-1)), gamma=5){
  ##################################################
  # Check the inputs
  if (!inherits(obj.fcp,"fcp")){
    stop("* nfcp : 'obj.fcp' should be an object from 'wrapfcp' function.")
  }
  
  ##################################################
  # Parameters
  mygamma = as.double(gamma)
  myKmax  = round(Kmax)
  myLmin  = 1
  
  ##################################################
  # Run the main function
  fcpout = dynprog_main(obj.fcp, Kmax=myKmax, Lmin=myLmin, gamma=mygamma)
  
  ##################################################
  # Prepare the output
  # 1. partition
  nobj = length(obj.fcp)
  partition = list()
  partition[[1]] = nobj
  for (i in 1:nrow(fcpout$Test)){
    partition[[i+1]] = sort(c(nobj, fcpout$Test[i,1:i]), decreasing = FALSE)
  }
  # 2. cost
  cost = as.vector(fcpout$obj$U)
  # 3. best partition
  best = partition[[which.min(cost)]]
  
  ##################################################
  # Wrap the output
  output = list()
  output$best = best
  output$cost = cost
  output$partition = partition
  return(output)
}

# 
# # Personal Example for Minimal Change Point Detection
# list.t = list()
# list.f = list()
# label  = c()
# for (i in 1:5){
#   tt = sort(rnorm(50))
#   list.t[[i]] = tt
#   list.f[[i]] = cos(tt) + rnorm(length(tt), sd=0.1)
#   label = c(label, 1)
# }
# for (i in 6:15){
#   tt = sort(rnorm(50))
#   list.t[[i]] = tt
#   list.f[[i]] = sin(2*tt) + rnorm(length(tt), sd=0.1)
#   label = c(label, 2)
# }
# for (i in 16:20){
#   tt = sort(rnorm(50))
#   list.t[[i]] = tt
#   list.f[[i]] = cos(3*tt)*sin(tt) + rnorm(length(tt), sd=0.1)
#   label = c(label, 3)
# }
# fcpobj <- wrapfcp(list.t, list.f)
# fcpout <- nfcp(fcpobj, Kmax=10, gamma=5)
# 
# # plot.x = fcpobj[[1]]$f
# # plot.x = (plot.x/abs(max(plot.x)))*0.5 + 1
# # plot.y = fcpobj[[1]]$t
# # plot(plot.x, plot.y)
# 
# x11(width=8, height = 6)
# plot(1:length(label), label, type="b",
#      xlab="index", main="change-point detection with gamma=5")
# abline(v=fcpout$best[1]+0.5, col="red", lwd=2)
# abline(v=fcpout$best[2]+0.5, col="red", lwd=2)
# abline(v=fcpout$best[3]+0.5, col="red", lwd=2)
# savePlot(filename="cp1.png")
# 
# x11(width=9,height=5)
# par(mfrow=c(1,3))
# plot(list.t[[1]], list.f[[1]], type="b", pch=19, cex=0.5, main="Class 1")
# plot(list.t[[10]], list.f[[10]], type="b", pch=19, cex=0.5, main="Class 2")
# plot(list.t[[20]], list.f[[20]], type="b", pch=19, cex=0.5, main="Class 3")
# savePlot(filename = "cp2.png")
