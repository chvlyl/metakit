
#### The boxplot layout has some issues, by column plot

wilcox_test = function(data.matrix,label.matrix,
                         parallel=FALSE,
                         ######
                         plot.heatmap=TRUE,
                         heatmap.top=0.05, #'all',5
                         heatmap.annotation=NA,
                         heatmap.log=FALSE,
                         heatmap.fsize=5,
                         ######
                         boxplot.top=5, ## all, 0.05
                         boxplot.y=NA,
                         boxplot.log=FALSE
                         ){
  
  data.matrix   <- as.matrix(data.matrix)
  label.matrix <- na.omit(label.matrix)
  
  #### only test one covariate
  if(ncol(label.matrix) != 1){stop('label matrix must have only one column')}
  
  ### make sure the two data matrix have the same samples
  ind <- intersect(rownames(data.matrix),rownames(label.matrix))
  data.matrix  <- data.matrix[ind,,drop=F]
  label.matrix <- label.matrix[ind,,drop=F]
  ### vector, instead of data frame
  rf.labels    <- label.matrix[,,drop=T]
  
  
  #### wilcoxon rank sum test
  wilcox.pq <- matrix(NA,nrow=ncol(data.matrix),ncol=14)
  rownames(wilcox.pq) <- colnames(data.matrix)
  ############################
  ## parallel
  if (parallel){
    ####
    #registerDoParallel(cores=5)
    cl<-makeCluster(5)
    registerDoParallel(cl)
    output.vec = foreach(feature=colnames(data.matrix),.combine='rbind',.packages="dplyr") %dopar% {
      pval <- wilcox.test(data.matrix[,feature]~rf.labels)$p.value
      mean.sd <- data.frame(value=data.matrix[,feature],label=rf.labels) %>% 
        na.omit() %>% group_by(label) %>% 
        summarise(median=median(value),mad=mad(value),
                  mean=mean(value),sd=sd(value),
                  present.prop=sum(value>0)/length(value),
                  median.present.abun=ifelse(sum(value>0)>0,median(value[value>0]),0))
      mean.sd.vec <- c(t(mean.sd[,-1])) #remove first column, label
      output.vec <- c(pval, signif(as.numeric(mean.sd.vec),4))
      output.vec <- as.matrix(output.vec)
      colnames(output.vec) <- feature
      rownames(output.vec) <- c('pval',  c(t(outer(mean.sd$label,colnames(mean.sd[,-1]),paste,sep='.'))) )
      t(output.vec)
      # In the following order (vector)
      # "g1.median" "g1.mad"    "g1.mean"   "g1.sd"  "g1.present.prop"  "g1.median.present.abu"
      # "g2.median" "g2.mad"    "g2.mean"   "g2.sd"  "g2.present.prop"  "g2.median.present.abu"
    }
    ### add row and col names
    rownames(wilcox.pq) <- rownames(output.vec)
    colnames(wilcox.pq) <- c(
      paste(colnames(label.matrix),c('pvalue','qvalue'),sep='.'),
      colnames(output.vec)[-1]
    )
    ### add values
    wilcox.pq[,1] <- output.vec[,1]
    wilcox.pq[,2] <- p.adjust(wilcox.pq[,1],method='fdr')
    wilcox.pq[,3:14] <- output.vec[,2:13]
    ########
  }
  ###################
  ## non-parallel
  else{
    for (feature in colnames(data.matrix)){
      wilcox.pq[feature,1] <- wilcox.test(data.matrix[,feature]~rf.labels)$p.value
      mean.sd <- data.frame(value=data.matrix[,feature],label=rf.labels) %>% 
        na.omit() %>% group_by(label) %>% 
        summarise(median=median(value),mad=mad(value),
                  mean=mean(value),sd=sd(value),
                  present.prop=sum(value>0)/length(value),
                  median.present.abun=ifelse(sum(value>0)>0,median(value[value>0]),0)
        ) 
      #browser()
      mean.sd.vec <- c(t(mean.sd[,-1])) #remove first column, label
      wilcox.pq[feature,3:14] <- signif(as.numeric(mean.sd.vec),4)
      ## In the following order (vector)
      ## "g1.median" "g1.mad"    "g1.mean"   "g1.sd"  "g1.present.prop"  "g1.median.present.abu"
      ## "g2.median" "g2.mad"    "g2.mean"   "g2.sd"  "g2.present.prop"  "g2.median.present.abu"
    }
    ########
    wilcox.pq[,2] <- p.adjust(wilcox.pq[,1],method='fdr')
    colnames(wilcox.pq) <- c(
      paste(colnames(label.matrix),c('pvalue','qvalue'),sep='.'),
      c(t(outer(mean.sd$label,colnames(mean.sd[,-1]),paste,sep='.')))
    )
  }
  
  ## http://stackoverflow.com/questions/10323817/r-unexpected-results-from-p-adjust-fdr
  ## However there is one more portion to the adjustment
  ## We don't want a p-value of .02 getting
  ## a larger q-value than a p-value of .05
  ## so we make sure that if a smaller q-value
  ## shows up in the list we set all of
  ## the q-values corresponding to smaller p-values
  ## to that corresponding q-value.
  
  #################################################
  #### boxplot
  if (!is.numeric(boxplot.top) & boxplot.top=='all'){
    boxplot.top <- nrow(wilcox.pq)
  }
  if (is.numeric(boxplot.top) & boxplot.top<1){
    boxplot.top <- sum(sort(wilcox.pq[,2])<0.05)
  }
  if (is.numeric(boxplot.top) & boxplot.top>0){
    if(nrow(wilcox.pq)<=boxplot.top){top.feature <- rownames(wilcox.pq)}
    else{top.feature <- names(sort(wilcox.pq[,2])[1:boxplot.top])}
    
    op <- par(no.readonly = TRUE)
    par(mfrow=c(2, 3), mar=c(4, 6, 4, 6), mgp=c(2, .8, 0),oma=c(0, 0, 2, 0), #(bottom, left, top, right) 
        xpd = NA,  # allow content to protrude into outer margin (and beyond)
        cex=0.6)  ## this cex control the overall size, the following cex change size based on this
    on.exit(par(op))
    for (fname in top.feature){
      if (boxplot.log){
        boxplot(log(data.matrix[,fname])~rf.labels,main=fname,ylab=boxplot.y,cex.main=0.8)
      }
      else{
        boxplot(data.matrix[,fname]~rf.labels,main=fname,ylab=boxplot.y,cex.main=0.8)
      }
      mtext(paste('p =',signif(wilcox.pq[fname,1],2),'q =',signif(wilcox.pq[fname,2],2)),cex=0.7)
    }
    
  }
  rm(top.feature)
  ##################################
  ### heatmap
  if (plot.heatmap){
    if (!is.numeric(heatmap.top) & heatmap.top=='all'){
      heatmap.top <- nrow(wilcox.pq)
    }
    if (is.numeric(heatmap.top) & heatmap.top<1){
      heatmap.top <- sum(sort(wilcox.pq[,2])<0.05)
    }
    if (is.numeric(heatmap.top) & heatmap.top>0){
      top.feature <- names(sort(wilcox.pq[,2])[1:heatmap.top])
      data.sig <- t(data.matrix[,top.feature])
      if(heatmap.log){data.sig <- log(data.sig)}
      if(is.na(heatmap.annotation)){heatmap.annotation <- label.matrix}
      if(nrow(data.sig) < 40){heatmap.fsize <- 10}
      pheatmap(data.sig,
               scale='none',fontsize_row = heatmap.fsize,
               clustering_distance_cols = "manhattan",
               cluster_cols = TRUE,
               cluster_rows = TRUE,
               annotation=heatmap.annotation
      )
    }
  }
  return(wilcox.pq)
}