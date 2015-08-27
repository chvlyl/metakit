#' Merge two tables by row or column names
#'
#' @param x A table
#' @param y A table
#' @return A merged table
#' @export
#' @examples




merge_two_tables = function(x,y,
                            fill.zero = TRUE,
                            rnames = 'union',
                            cnames = 'intersect'
                            ){
  
  if(rnames == 'union'){
    rnames.new <- union(rownames(x),rownames(y))
  }
  else if(rnames == 'intersect'){
    rnames.new <- intersect(rownames(x),rownames(y))
  }
  else{stop('rowname option is not right')}
  
  if(cnames == 'union'){
    cnames.new <- union(colnames(x),colnames(y))
  }
  else if(cnames == 'intersect'){
    cnames.new <- intersect(colnames(x),colnames(y))
  }
  else{stop('colname option is not right')}
  
  matrix.new <- matrix(NA,ncol=length(cnames.new),nrow=length(rnames.new))
  colnames(matrix.new) <- cnames.new
  rownames(matrix.new) <- rnames.new
  
  rindx <- intersect(rnames.new,rownames(x))
  cindx <- intersect(cnames.new,colnames(x))
  matrix.new[rindx,cindx] <- x[rindx,cindx] 
  
  rindy <- intersect(rnames.new,rownames(y))
  cindy <- intersect(cnames.new,colnames(y))
  matrix.new[rindy,cindy] <- y[rindy,cindy] 
  
  if(fill.zero){matrix.new[is.na(matrix.new)] <- 0 }
  
  return(matrix.new)
  
}