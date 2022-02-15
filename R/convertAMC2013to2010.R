#' Função para carregar os dados a partir dos arquivos
#'
#' Esta função carrega os dados existentes em um diretório. Os dados precisam estar formatados conforme disponibilizados pela PNUD
#'
#' No ano de 2013 foram criados 5 municípios a partir do desmembramento de territórios
#' municipais. Análises longitudinais ou estudos transversais que utilizem variáveis de períodos
#' Identificando os municípios novos
#' ```regAdm[!(unique(regAdm$IBGE7) %in% unique(censo$Codmun7)),c("IBGE7","NOME")]```
#' Pesquisa de onde o município foi desmembrado e localiza o código do IBGE
#' ```subset(regAdm, NOME == "Santarém" & substr(regAdm$IBGE7,1,2)=="15", select = c("ANO","IBGE7","NOME"))
#' 1504752(Mojuí dos Campos)  -> 1506807(Santarém)
#' 4212650(Pescaria Brava)    -> 4209409(Laguna)
#' 4220000(Balneário Rincão)  -> 4207007(Içara)
#' 4314548(Pinto Bandeira)    -> 4302105(Bento Gonçalves)
#' 5006275(Paraíso das Águas) ->5002951(Chapadão do Sul)```
#' OBS: O município foi desmembrado utilizando área de 3 municípios (Água Clara,
#' Costa Rica e Chapadão do Sul) no entanto optou-se por agrega-lo ao município mais populoso;
#'
#'
#'
convertAMC2013to2010 <- function(dataframe, codMun = "IBGE7",nomeMun = "NOME"){
 filtro = parse(text=paste("dataframe",codMun,sep = '$'))
 dataframe[eval(filtro) == "1504752",c(`codMun`,`nomeMun`)] = list("1506807","Santarém")
 dataframe[eval(filtro) == "4212650",c(`codMun`,`nomeMun`)] = list("4209409","Laguna")
 dataframe[eval(filtro) == "4220000",c(`codMun`,`nomeMun`)] = list("4207007","Içara")
 dataframe[eval(filtro) == "4314548",c(`codMun`,`nomeMun`)] = list("4302105","Bento Gonçalves")
 dataframe[eval(filtro) == "5006275",c(`codMun`,`nomeMun`)] = list("5002951","Chapadão do Sul")

 colCodMun <- eval(parse(text=paste("dataframe",codMun,sep = '$')))
 colNomeMun <- eval(parse(text=paste("dataframe",nomeMun,sep = '$')))


 return(

  aggregate(x=dataframe[,-(1:2)],by = list(codmun = colCodMun,municipio = colNomeMun),
            FUN = sum)
  )
}

#dataframe <- merge(censo,novo,by.x = c("Codmun7","Município"),by.y = c("codmun","municipio"),all.y = T)
