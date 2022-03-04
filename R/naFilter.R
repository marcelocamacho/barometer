#' Filter collumns by NA ratio
#'
#' Select columns that have NA ratio above a threshold. A vector will be
#' returned with the indices of the columns that exceeded the limit.
#'
#' @param x Dataframe
#' @param percentCases threshold

naFilter <- function(x, percentCases=0.5){
 index <- NA
 for(i in 1:ncol(x)){
  nna = sum(is.na(x[,i]))

  if(nna/nrow(x)<=percentCases){
   index=append(index,i)
  }
 }
 index=index[-is.na(index)]

  ifelse(length(index)<1,
   return(1:ncol(x)),
   return(index)
  )

}
