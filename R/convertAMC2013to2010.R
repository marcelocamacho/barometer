#' Function to load data from files
#'
#' This function loads existing data into a directory. Data must be formatted as made available by PNUD
#'
#' In 2013, 5 municipalities were created from the dismemberment of municipal
#' territories. Longitudinal analyzes or cross-sectional studies that use period variables.
#' 1504752(Moju\\u00ed dos Campos)  -> 1506807(Santar\\u00e9m)
#' 4212650(Pescaria Brava)    -> 4209409(Laguna)
#' 4220000(Balne\\u00e1rio Rinc\\u00e3o)  -> 4207007(I\\u00e7ara)
#' 4314548(Pinto Bandeira)    -> 4302105(Bento Gon\\u00e7alves)
#' 5006275(Para\\u00edso das \\u00c1guas) ->5002951(Chapad\\u00e3o do Sul)```
#' OBS: The municipality was split using an area of 3 municipalities (\\u00c1gua Clara, Costa Rica and Chapad\\u00e3o do Sul), however it was decided to add it to the most populous municipality;
#'
#' @param dataframe data with municipal records that will be normalized
#' @param codMun name of the column that stores the IBGE code of the municipality
#' @param nomeMun column name that stores the municipality name
#'
#' @return dataframe
#'
#' @export
#'
convertAMC2013to2010 <- function(dataframe, codMun = "IBGE7",nomeMun = "NOME"){
 filtro = parse(text=paste("dataframe",codMun,sep = '$'))
 dataframe[eval(filtro) == "1504752",c(`codMun`,`nomeMun`)] = list("1506807",stringi::stri_unescape_unicode("Santar\\u00e9m"))
 dataframe[eval(filtro) == "4212650",c(`codMun`,`nomeMun`)] = list("4209409","Laguna")
 dataframe[eval(filtro) == "4220000",c(`codMun`,`nomeMun`)] = list("4207007",stringi::stri_unescape_unicode("I\\u00e7ara"))
 dataframe[eval(filtro) == "4314548",c(`codMun`,`nomeMun`)] = list("4302105",stringi::stri_unescape_unicode("Bento Gon\\u00e7alves"))
 dataframe[eval(filtro) == "5006275",c(`codMun`,`nomeMun`)] = list("5002951",stringi::stri_unescape_unicode("Chapad\\u00e3o do Sul"))

 colCodMun <- eval(parse(text=paste("dataframe",codMun,sep = '$')))
 colNomeMun <- eval(parse(text=paste("dataframe",nomeMun,sep = '$')))

 return(
  #stats::aggregate(x=dataframe[,-(1:4)],by = list(IBGE7=colCodMun,NOME=colNomeMun),FUN = sum)
  dplyr::group_by(.data = dataframe,IBGE7,NOME) %>%
   dplyr::select(-ANO,-DESAGREGACAO) %>%
   dplyr::summarise(across(where(is.numeric),sum))
  )
}

