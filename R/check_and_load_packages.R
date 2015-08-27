#' Install and load all the necessary packages
#'
#' @export
#' @examples
#' load_packages(pkgs="dplyr")
#' 
#' 
#' 


##### NOT USE ####
##### Need specific treatment for loading GitHub packages when the name has "/" in it

load_packages <- function(pkgs){
#   for (pk in pkgs){
#     if (!require(pk,character.only = TRUE)){
#       #### GitHub Packages
#       if(grepl("/",pk)){
#         install_github(pk)
#       }
#       #### CRAN packages
#       else{
#         install.packages(pk,dep=TRUE)
#       }
#     }
#     require(pk,character.only = TRUE,quietly=TRUE)
#   }
}