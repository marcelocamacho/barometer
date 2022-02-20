evaluate<-function(x){
 eval(parse(text=x))
}


createDimensions <- function(dimentions=c("Ambiental", "Humana")){

 print(">> APLICAÇÃO DO BARÔMETRO DA SUSTENTABILIDADE <<")

 if(is.null(dimentions)||length(dimentions)<2){
  message("You must provide at least two dimensions")
  exit()
 }

 for(di in dimentions){
  x <- readline(prompt =
                 paste("Selecione as variáveis da dimensão",di,"(separado por vírgula):",sep = ' '))
  x<-unlist(strsplit(x,split = ','))

  if(is.null(x)||length(x)<3||is.na(x)){
   errorCondition("É necessário incluir uma variável válida para o indicador")
  }

  assign(di,
         setNames(
          data.frame(
           matrix(ncol = length(x), nrow = 0)
          ), x),.GlobalEnv)

 }
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

  message(
   sprintf("A base de dados %s possui os anos %s.\n A base de dados %s possui os anos %s.\n",
          quote("'CENSO'"),
          paste(unique(censo$ANO),collapse = ','),
          quote("'Registros Administrativos'"),
          paste(unique(regAdm$ANO),collapse = ','))

  )

  anoCenso <-readline("Selecione o ano da base CENSO: ")
  anoRegAdm <-readline("Selecione o ano da base REGISTROS ADMINISTRATIVOS: ")

  censo <- dplyr::filter(.data=censo,ANO==as.numeric(anoCenso))
  regAdm <- dplyr::filter(.data=regAdm,ANO==as.numeric(anoRegAdm))

  message("Os anos selecionados são diferentes e podem implicar em diferenças na quantidade e nome de municípios.")
  runConverteAMC<-readline("Deseja converter a configuração territorial para o ano de 2010 para uniformizar o nome dos municípios?  [(S)im/(n)ão")

  if(runConverteAMC %in% c('S','s','Sim','sim')){
   regAdm<-convertAMC2013to2010(regAdm)
  }



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


#assign(x = "censo",value = censo, envir = .GlobalEnv)
#assign(x = "regAdm",value = regAdm, envir = .GlobalEnv)

}

