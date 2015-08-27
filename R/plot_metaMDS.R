#' plot MDS results from metaMDS function of vegan package
#' 
#' @param X the output from metaMDS function, must be a 'metaMDS' class
#' @param sample.labels a data frame with sample types which will be used in the plot
#' @param title
#' @export
#' 

####!!!!!!!!!!!!!!!!!!!!!!##############
#### not working ##########
####!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



### first variable -> color
### second variable -> shpae
### third variable -> third
plot_metaMDS <- function(meta.MDS, MDS.sample.info,
                         col.var=NA,
                         shape.var=NA,
                         size.var=NA,
                         title='metaMDS'){
  if(class(meta.MDS)[1] != 'metaMDS'){
    stop('This funtion only works for output from metaMDS (vegan package)')
  }
  ### add sample information
  meta.MDS.plot <- add_rownames(as.data.frame(meta.MDS$points),var='Sample') 
  if(!is.na(col.var)){
    meta.MDS.plot <- meta.MDS.plot %>%
      left_join(dplyr::select(
        add_rownames(MDS.sample.info,var='Sample'),
        Sample,get(col.var)),
      by='Sample') 
  }
  if(!is.na(shape.var)){
    meta.MDS.plot <- meta.MDS.plot %>%
      left_join(dplyr::select(
        add_rownames(MDS.sample.info,var='Sample'),
        Sample,get(shape.var)),
      by='Sample') 
  }
  if(!is.na(size.var)){
    meta.MDS.plot <- meta.MDS.plot %>%
      left_join(dplyr::select(
        add_rownames(MDS.sample.info,var='Sample'),
        Sample,get(size.var)),
        by='Sample') 
  }
  
  
  ### plot
  p <- ggplot(meta.MDS.plot,aes(x=MDS1,y=MDS2))+ 
    geom_point(aes(colour = get(col.var))) +
    geom_point(aes(shape = get(shape.var)))  +
    scale_color_manual(values=c("#0066CC", "#FF3333"),name="") +
    scale_shape_manual(values=c(20,4),name="")+
    labs(title = 'metaMDS') 
    #print(p)
  p <- ggplot(meta.MDS.plot,
              aes(x=MDS1,y=MDS2,
                  colour=(ifelse(!is.na(col.var),get(col.var),'blue')),shape=Cluster))+ 
    geom_point(aes(size = Human.Per)) + 
    scale_color_manual(values=c("#0066CC", "#FF3333"),name="") + 
    #scale_color_manual(values=PercentHuman,name="") + 
    #scale_colour_gradient(low = "blue") + 
    scale_shape_manual(values=c(20,1),name="") + labs(title = 'metaMDS') 
  print(p)
  if(!is.na(col.var)){
     
  }
  
  if(!is.na(shape.var)){
    
      
      
      #
  }
  
#   ### color
#   if (ncol(meta.MDS.plot) > 3){
#     p <- p + geom_point(aes(colour = get(colnames(meta.MDS.plot)[4]))) + 
#       scale_color_manual(values=c("#0066CC", "#FF3333"),name="")
#   }
#   ### shape
#   if (ncol(sample.info)>1){
#     p + geom_point(aes(shape = Cluster)) + 
#       scale_shape_manual(values=c(20,1),name="") 
#   }
#   ### size
#   if (ncol(sample.info)>2){
#     p + geom_point(aes(size = Fungi.Per))
#   } 
#  print(p)
}


# plot_metaMDS <- function(X,sample.labels,title='metaMDS'){
#   if(class(meta.MDS)[1] != 'metaMDS'){
#     stop('This funtion only works for output from metaMDS (vegan package)')
#   }
#   plot.Data <- as.data.frame(matrix(NA,nrow=nrow(X$points),ncol=2+ncol(sample.labels)))
#   colnames(plot.Data) <- c('MDS1','MDS2',colnames(sample.labels))
#   rownames(plot.Data) <- rownames(X$points)
#   plot.Data[,c(1,2)]  <- X$points
#   plot.Data[rownames(sample.labels),colnames(sample.labels)] <- sample.labels
#   
#   qq.plot = qplot(MDS1,MDS2,data = plot.Data,
#                   colour=get(colnames(sample.labels)[1]),
#                   shape=get(colnames(sample.labels)[2]))+ 
#     scale_color_manual(values=c("#0066CC", "#FF3333"),name="") +
#     scale_shape_manual(values=c(20,4),name="") + labs(title = title) 
#   print(qq.plot)
# }