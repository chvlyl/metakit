#' Linear regression with permutation
#'
#' @param data.matrix 
#' @param sample.info 
#' @param condition
#' @return data matrix that subset by condtion
#' @export
#' @examples



linear_regression_with_permutation_ = function(formula,data.mat,
                                               perm.var,
                                               perm.num=1000){  
  
  formula <- lazy_eval(formula,data.mat)
  #### true model
  lm.model <- lm(formula,data=data.mat)
  true.t <- coef(summary(lm.model))[,3]
  
  ### permutation
  null.num <- matrix(NA,nrow=perm.num,ncol=length(true.t))
  colnames(null.num) <- rownames(coef(summary(lm.model)))
  for (i in 1:perm.num){
    data.mat[,perm.var] <- sample(data.mat[,perm.var],nrow(data.mat))
    lm.model.perm <- lm(formula,data=data.mat)
    perm.t <-coef(summary(lm.model.perm))[,3]
    null.num[i,] <- abs(perm.t) > abs(true.t)
  }
  p.perm <- colSums(null.num)/perm.num
  
  model.est <- signif(data.frame(coef(summary(lm.model)),pval.perm = p.perm),2)
  return(model.est)
}

linear_regression_with_permutation=function(formula,data.mat,perm.var,perm.num=1000){ 
  linear_regression_with_permutation_(lazy(formula), data.mat, perm.var,perm.num=1000)
}