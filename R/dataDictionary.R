#' Data dictionary for the packet data
#'
#' The package data is named by acronyms and this package provides the data
#'dictionary containing the acronym, the indicator name and a more detailed description.
#'
#' @param text search string. Pattern.
#' @param ... further arguments passed to or from other methods.
#'
#'@export
dataDictionary <- function(text=NULL,...){

 censoPath <- system.file("extdata",
                          "Atlas_2013_Censo_municipal_estadual_Brasil.xlsx",package = "barometer")
 regAdmPath <- system.file("extdata",
                           "Atlas_2013_RegistrosAdministrativos.xlsx", package = "barometer")

 censo <- readxl::read_excel(censoPath,
                             sheet = "Siglas"
 )
 regAdm <- readxl::read_excel(regAdmPath,
                              sheet = "LEGENDA"
 )
 censo$source <- "CENSO"
 regAdm$source <- "REGISTROS ADMINISTRATIVOS"

 censo <- censo %>% dplyr::select(-`NOME CURTO`)
 names(censo) <- c("acronym", "indicated", "definition","dataSource")

 names(regAdm) <- c("acronym", "indicated", "definition","dataSource")

 dictionary <- rbind(censo,regAdm)

 if(!is.null(text)){
  data<-do.call(paste,dictionary)
  idx <- grep(text, data)
  dictionary <- dictionary[idx,]
 }

  dictionary

}
