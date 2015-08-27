#' PERMANOVA
#'
#' @param dist.mat
#' @param cov.mat
#' @return A merged table
#' @export
#' @examples






PERMANOVA_analysis_ = function(dist.mat,cov.mat){
  cov.mat <- add_rownames(cov.mat,var='Sample') %>%
    inner_join(data.frame(Sample=labels(dist.mat)),by='Sample') %>%
    na.omit %>%
    add_back_rownames(row.var='Sample')
  dist.mat <- as.dist(as.matrix(bact.dist.cp1)[rownames(cov.mat),rownames(cov.mat)])
  
  ### use the colnames of cov.mat as the covariates
  formula <- paste('dist.mat ~',paste(colnames(cov.mat),sep='+'))
  ### must used dist.mat as the distance name in the formula
  set.seed(100)
  adonis(lazy_eval(formula), data = cov.mat,perm=1000)$aov.tab[['Pr(>F)']][1]
}


PERMANOVA_analysis = function(dist.mat,cov.mat){
  PERMANOVA_analysis_(dist.mat,cov.mat)
}