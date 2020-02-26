#' Wrap Functional Data for FCP Package
#' 
#' 
#' @export
wrapfcp <- function(list.t, list.f){
  ##################################################
  # Check
  # 1. both should be lists of same length
  cond1 = (is.list(list.t))
  cond2 = (is.list(list.f))
  cond3 = (length(list.t)==length(list.f))
  if (!(cond1&&cond2&&cond3)){
    stop("* wrapfcp : two inputs 'list.t' and 'list.f' should be lists of same length.")
  }
  # 2. every element should be of same-length vector.
  N = length(list.t)
  for (n in 1:N){
    tn = list.t[[n]]
    fn = list.f[[n]]
    cond1 = is.vector(tn)
    cond2 = is.vector(fn)
    cond3 = (length(tn)==length(fn))
    if (!cond1){
      stop(paste0("* wrapfcp : ",n,"-th element of 'list.t' is not a vector."))
    }
    if (!cond2){
      stop(paste0("* wrapfcp : ",n,"-th element of 'list.f' is not a vector."))
    }
    if (!cond3){
      stop(paste0("* wrapfcp : ",n,"-th elements of 'list.t' and 'list.f' do not have same length."))
    }
  }
  # 3. capsulate
  output = list()
  for (n in 1:N){
    tmp = list()
    tmp$t = list.t[[n]]          # x axis
    tmp$f = list.f[[n]]          # y axis
    tmp$n = length(list.t[[n]])  # length of the vector
    output[[n]] = tmp
  }
  class(output) = "fcp"
  return(output)
}

# # Personal Example to Create 'fcp' object
# list.t = list()
# list.f = list()
# for (i in 1:10){
#   tt = sort(rnorm(20))
#   list.t[[i]] = tt
#   list.f[[i]] = cos(tt) + rnorm(length(tt), sd=0.1)
# }
# fcpobj <- wrapfcp(list.t, list.f)
