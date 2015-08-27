#' Calculate the distance for a data matrix
#' 
#' @param dist the distance used for clustering
#' @param plot.results TRUE or FALSE
#' @return the best clustering
#' @export
#' 


find_best_clustering <- function(dist,plot.results=TRUE){
  asw <- numeric(20)
  for (k in 2:20){
    asw[k] <- pamk(dist, k)$pamobject$silinfo$avg.width
  }
  k.best <- which.max(asw)
  cat("Silhouette-optimal number of clusters:", k.best, "\n")
  if(plot.results){
    plot(1:20, asw, type= "h", main = "PAM clustering assessment",
         cex.lab=2.0, cex.main=2.0, cex.axis=1.2, 
         xlab= "k  (# clusters)", ylab = "Average silhouette width")
    axis(1, k.best, paste("best",k.best,sep="\n"), col = "red", col.axis = "red", cex.axis=1.0)
  }
  
  pamk.best <- pamk(dist,krange=2:5,criterion="asw", usepam=TRUE,
                    scaling=FALSE, alpha=0.001, diss=TRUE ,
                    critout=FALSE, ns=10)
  return(pamk.best)
}