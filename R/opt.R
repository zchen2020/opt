
#' Optimal P-value combination Test (OPT)
#'
#' @param p p-values to be combined
#' @param lower.alpha minimum alpha value considered in the test
#' @param upper.alpha maximum alpha value considered in the test
#' @param rep number of replicates to estimate the p-value using resampling
#'
#' @return the estimated test statistic, the alpha and the c values, the p-value of the test
#' @export
#'
#' @examples p=c(0.1,0.2,0.3)
#' opt(p,rep=1e3)
#'
opt<-function(p,lower.alpha=1e-2,upper.alpha=1e8, rep=1e4){
  stat.opt<-opt_test(p=p,l.alpha=lower.alpha,u.alpha=upper.alpha)
  stat<-stat.opt[1]
  alpha<-stat.opt[2]
  c<-stat.opt[3]

  # estiamte p-values (constrained) using re-sampling method:
  if (stat==Inf) { p.value<-1e-300} else{
    ## estiamte p-values:
    s.const<-rep(1,rep)
    n<-length(p)
    for (i in 1:rep){
      pp<-runif(n,0,1)
      ress<-opt_test(p=pp,l.alpha=lower.alpha,u.alpha=upper.alpha)
      s.const[i]<-ress[1]
    }
    p.value<-length(s.const[s.const>stat])/rep
  }
  results<-list(stat=stat,alpha=alpha,c=c, p.val= p.value)
  return(results)
}
