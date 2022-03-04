#' Evaluate a metaexpression
#'
#' In case it is necessary to evaluate a string as a variable
#'
#' At various times, arguments are expected from functions that are later evaluated
#' as objects or even functions. This function is a contracted form for the
#' combination of eval(parse(text="str")).
#' @param x string that will be evaluated
#' @param ... Other optional arguments

evaluate<-function(x,...){
 eval(parse(text=x))
}
