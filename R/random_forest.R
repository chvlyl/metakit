#' Random Forest 
#'
#' @param data.matrix 
#' @param label.matrix
#' @return model
#' @export
#' @examples


random_forest = function(data.matrix,label.matrix,
                             plot.MDS=TRUE,plot.importance=TRUE,print.result=TRUE,
                             title=NA,guess.num=20,
                             boxplot.top=5,boxplot.y=NA,
                             seed=100){
  cgreen <- rgb(77,175,74,maxColorValue =255)
  cpurple <- rgb(152,78,163,maxColorValue =255)

  #library('randomForest')
  data.matrix   <- as.matrix(data.matrix)
  label.matrix <- as.data.frame(label.matrix)
  
  ### make sure the two data matrix have the same samples
  ind <- intersect(rownames(data.matrix),rownames(label.matrix))
  data.matrix   <- data.matrix[ind,]
  label.matrix <- label.matrix[ind,,drop=F]
  
  for (label in colnames(label.matrix)){
    print(label)
    rf.labels <- as.factor(label.matrix[,label,drop=T])
    set.seed(seed)
    rf.model <- randomForest(data.matrix,rf.labels,
                             importance=TRUE,proximity=TRUE, na.action=na.omit)
    if(print.result){
      print(rf.model)
    }
    if(plot.importance){
      ## Plot importance score
      varImpPlot(rf.model,cex=0.5,main='Importance scores')
    }
    if(plot.MDS){
      ## Plot MDS
      MDSplot(rf.model,as.factor(rf.labels),palette=c(cgreen,cpurple),pch=as.numeric(rf.labels),
            main='Random froest MDS plot')
      legend('topleft',legend=levels(rf.labels),col=c(cgreen,cpurple),
             pch=unique(as.numeric(rf.labels)),cex=0.5)
    }
    
    ################
    ###
    if (guess.num>0){
      rf.error <- rf.model$err.rate[rf.model$ntree, "OOB"]
      rf.error.per <- rep(NA,guess.num)
      for (i in 1:guess.num){
        set.seed(seed+i)
        rf.labels.per <- sample(rf.labels,length(rf.labels))
        set.seed(seed*5+i)
        rf.model.per  <- randomForest(data.matrix,rf.labels.per,
                                 importance=TRUE,proximity=TRUE, na.action=na.omit)
        rf.error.per[i] <- rf.model.per$err.rate[rf.model.per$ntree, "OOB"]
      }
      pvalue <- sum(rf.error.per<=rf.error)/guess.num
      cat('Permutation error: mean',round(mean(rf.error.per),2),'sd',round(sd(rf.error.per),2),
          'real data error',round(rf.error,2),'\n',
          'pvalue',pvalue,'\n')
    }
    ###############
    #### boxplot
    if (boxplot.top>0){
      ### model$importance and importance(model) give different results
      ### the varImpPlot uses the importance(model)
      top.feature <- names(sort(importance(rf.model)[,'MeanDecreaseAccuracy'],decreasing=TRUE)[1:boxplot.top])
      op <- par(no.readonly = TRUE)
      par(mfrow=c(2, 3), mar=c(4, 5, 4, 1), mgp=c(2, .8, 0),oma=c(0, 0, 2, 0))
      on.exit(par(op))
      for (fname in top.feature){
          boxplot(data.matrix[,fname]~rf.labels,main=fname,ylab=boxplot.y,cex.main=0.8)
      }
      
    }
  }
  return(rf.model)
}