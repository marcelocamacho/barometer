#' Create Dimensions
#'
#' Creates the dataframe with the dimensions and variables that will be computed in the sustainability barometer
#'
#' The sustainability barometer is a multidimensional metric, therefore, it needs
#' a minimally two-dimensional structure to be computed. The input dataframe is
#' organized in a tabular way formed by the dimension, variable and value attributes.
#' The record (row) of the dataframe is the territories for which the indicators
#' will be calculated.
#'
#' @param dimensions It must have at least two dimensions, if not informed, the
#' two-dimensional proposition composed of "Environment" and "Human" will be adopted.
#' @param variables Database variables that will be included in dimensions
#' @param ... Other optional arguments
#'
#' @return dataframe
#'
#' @export
#'
createDimensions <- function(dimensions=c("Ambiental", "Humana"),...,variables){

 if(is.null(dimensions)||length(dimensions)<2){
  message("You must provide at least two dimensions")
  sys.on.exit()
 }

 for(di in dimensions){
  x <- readline(prompt =
                 paste("Select dimension variables",di,"(separated by comma):",sep = ' '))
  x<-unlist(strsplit(x,split = ','))

  if(is.null(x)||length(x)<3||is.na(x)){
   errorCondition("It is necessary to include a valid variable for the indicator")
  }

  assign(di,
         stats::setNames(
          data.frame(
           matrix(ncol = length(x), nrow = 0)
          ), x))

 }
}

