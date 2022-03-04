#' Function to load raw data from files
#'
#' This function loads existing data into a directory.
#' Data must be formatted as made available by PNUD.
#'
#' Data source: http://www.atlasbrasil.org.br/acervo/biblioteca
#'
#' @importFrom rlang .data
#' @param ... optional arguments
#' @param runConverteAMC maintains a minimum area of comparison for different territorial configurations of the database
#' @param yearCensus year of census data. Includes 2000 and 2010.
#' @param yearRegAdm year of administrative records data. Includes 2013 to 2016.
#'
#' @return None
#'
#' @export
loadDataFromFiles <- function(...,
                              runConverteAMC=TRUE,
                              yearCensus=2010,
                              yearRegAdm=2013 ){
 censoPath <- system.file("extdata",
                    "Atlas_2013_Censo_municipal_estadual_Brasil.xlsx",package = "barometer")
 regAdmPath <- system.file("extdata",
                     "Atlas_2013_RegistrosAdministrativos.xlsx", package = "barometer")

  censo <- readxl::read_excel(censoPath,
               sheet = "MUN 91-00-10",
               col_types = c(rep("numeric",2), rep("text",3),rep("numeric",232))
           )
  regAdm <- readxl::read_excel(regAdmPath,
               sheet = stringi::stri_unescape_unicode("MUNIC\\u00cdPIO"),
               col_types = c("numeric", rep("text",3), rep("numeric",78))
            )

  censo <- dplyr::filter(.data=censo,ANO==as.numeric(yearCensus))
  regAdm <- dplyr::filter(.data=regAdm,ANO==as.numeric(yearRegAdm))

  if(runConverteAMC){
   regAdm<-convertAMC2013to2010(regAdm)
  }

  if(nrow(censo)!=nrow(regAdm)){
   message("Number of different lines")
  }

  censo <- censo[,naFilter(censo)]
  regAdm <- regAdm[,naFilter(regAdm)]


  #Utilizando o nome das variáveis diretamente. Isso poderá dar problema se for trocado.
  dataframe <-
   merge(x=censo,y=regAdm,
                    by.x = "Codmun7",
                    by.y = "IBGE7",
                    all = F)
   #dplyr::inner_join(censo,regAdm,
   #                  by=c("Codmun7" = "IBGE7"))


  dataframe <- dataframe[,!names(dataframe) %in% c("ANO","Codmun6")];

  return(dataframe)

}

#' Updates the data used in the package
#'
#' The data used are made available by autonomous data sources such as IBGE,
#' IPEA, UNDP and others. Whenever there is the possibility of updating these
#' sources, this function updates the internal Rdata used by the
#' sustainability barometer.
#'
#'
renewRD <- function(){
 data <- loadDataFromFiles()
 usethis::use_data(data,internal = TRUE)
}




