
#' OPT test
#'
#' @param p p-values to be combined
#' @param l.alpha lower.alpha minimum alpha value considered in the test
#' @param u.alpha maximum alpha value considered in the test
#'
#' @return the estimated test statistic, the alpha and the c values
#' @export
#'
#' @examples p=c(0.1,0.2,0.3)
#' opt_test(p)
#'
#'
#'
#'
opt_test<-function(p,l.alpha=1e-2,u.alpha=1e8){
  # six starting points:
  start<-matrix(c(1e-2,1e-2,
                  1e-2,1e-1,
                  1e2,1e-2,
                  1e2,1e-1,
                  1,1e-2,
                  1,1e-1),
                6,2,byrow = TRUE)
  # log-likelihood with two parameters (alpha and c)

  if( any(p < 0 | p > 1) ) stop('p not between 0 and 1')

  l2<-function(theta) {-theta[1]*length(p)*log(1-theta[2])-theta[2]*sum(qgamma(1-p,theta[1]))}
  # try different starting values:
  obj.const<-rep(0,6)
  k<-nrow(start)
  for (i in 1:k){
    obj.const[i]<-nlminb(start[i,],l2,lower=c(l.alpha,0),upper=c(u.alpha,1))$objective
  }
  s.const<-order(obj.const)[1]  # select parameter base on the smallest -ll value (excude -Inf)
  # constrained LRT test (assuming 0<c<1 in finding MLE):
  res.const<-nlminb(start[s.const,],l2,lower=c(l.alpha,0),upper=c(u.alpha,1))
  stat.const<--2*res.const$objective
  alpha.const<-res.const$par[1]
  c.const<-res.const$par[2]
  return(c(stat.const,alpha.const,c.const)) # be careful of the order of the output: test statitics, estimated alpha, and estimated c
}

