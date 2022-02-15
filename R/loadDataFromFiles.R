
createDimensions <- function(){

 print(">> APLICAÇÃO DO BARÔMETRO DA SUSTENTABILIDADE <<")
 dimentions <- readline(prompt = "Quais as dimensões que voce deseja criar?\n Nomes separados por vírgula. \nPor exemplo: Ambiental, Humana, Social")

 if(is.null(dimentions)||length(dimentions)<3){
  dimentions<-"Humana,Ambiental"
 }

 dimentions <- unlist(strsplit(dimentions,split = ','))
 print(dimentions)
 for(i in dimentions){
  x <- readline(prompt =
                 paste("Selecione as variáveis da dimensão",i,":",sep = ' '))
  if(is.null(x)||length(x)<3||is.na(x)){
   errorCondition("É necessário incluir uma variável válida para o indicador")
  }


 }
 variaveis

}




#' Função para carregar os dados a partir dos arquivos
#'
#' Esta função carrega os dados existentes em um diretório.
#' Os dados precisam estar formatados conforme disponibilizados pela PNUD.
#'
#' @param diretorio Uma string com o caminho para o diretório desejado.
#'
#'
loadDataFromFiles <- function(diretorio = "data"){
  censo <- readxl::read_excel(
   paste(diretorio,
         "registrosCenso/Atlas 2013_municipal, estadual e Brasil.xlsx",sep = '/'),
                      sheet = "MUN 91-00-10", col_types = c(
                       rep("numeric",2), rep("text",3),rep("numeric",232)))
  regAdm <- readxl::read_excel(
   paste(diretorio,
         "registrosAdministrativos/DOWNLOAD REGISTRO ADMINISTRATIVO TOTAL 2012 A 2017.xlsx",sep = '/'),
                      sheet = "MUNICÍPIO",
                      col_types = c("numeric",
                       rep("text",3),
                       rep("numeric",78)))


  censo <- dplyr::filter(.data=censo,ANO==2010)
  regAdm <- dplyr::filter(.data=regAdm,ANO==2013)

#  censo <- dplyr::select(.data = censo,Codmun7,'Município',
#                         MORT1,
#                         PIND,
#                         T_ATIV,
#                         T_ATIV1014,
#                         RDPC,
#                         T_ANALF15M,
#                         T_LUZ,
#                         GINI,
#                         T_AGUA,
#                         T_BANAGUA,
#                         T_LIXO)
#  regAdm <- dplyr::select(.data = regAdm, IBGE7,NOME,
#                          POP_TOT,
#                          TXMOINF,
#                          TXOBITMATERN,
#                          TXMAE10A14,
#                          REN_PIBPC_D,
#                          IDEB_AI,
#                          IDEB_AF,
#                          TTREVA_EF_TOTAL,
#                          TTREVA_EM_TOTAL,
#                          TXMOHOMI,
#                          PFOCOS)


assign(x = "censo",value = censo, envir = .GlobalEnv)
assign(x = "regAdm",value = regAdm, envir = .GlobalEnv)

}

