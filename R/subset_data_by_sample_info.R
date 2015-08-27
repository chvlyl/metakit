#' Subset data by sample information
#'
#' @param data.matrix 
#' @param sample.info 
#' @param condition
#' @return data matrix that subset by condtion
#' @export
#' @examples


subset_data_by_sample_info_ = function(data.matrix,sample.info,condition){
  r <- lazy_eval(condition, sample.info)
  r <- r & !is.na(r)
  data.matrix[rownames(data.matrix) %in% rownames(sample.info[r, , drop = FALSE]),,drop=FALSE]
}

subset_data_by_sample_info = function(data.matrix,sample.info,condition){
  subset_data_by_sample_info_(data.matrix,sample.info,lazy(condition))
}

## http://cran.r-project.org/web/packages/lazyeval/vignettes/lazyeval.html