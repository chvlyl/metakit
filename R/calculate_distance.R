#' Calculate the distance for a data matrix
#' 
#' @param X matrix
#' @param method 'UniFrac','Binary Jaccard' or 'Numberical Jaccard'
#' @param tree the phlogenetic tree information for UniFrac
#' @return  a distance
#' @export
#' 


calculate_distance <- function(X,method,tree=NA){
  if (method == 'UniFrac'){
    if (is.na(tree)){stop('Need the tree information to run UniFrac!')}
    #overlap.species <- intersect(colnames(X),tree$tip.label)
    unifracs <- GUniFrac(X,tree,alpha=0.5)
    dist <- as.dist(unifracs$unifracs[,,1])
  }
  else if (method == 'Binary_Jaccard'){
    dist <- vegdist(X,method='jaccard',binary=TRUE)
  }
  else if (method == 'Numerical_Jaccard'){
    dist <- vegdist(X,method='jaccard',binary=FALSE)
  }
  else if (method == 'Bray'){
    dist <- vegdist(X,method='bray',binary=FALSE)
  }
  else {
    stop('method is not correct')
  }
  return(dist)
}