library(barometer)
library(dplyr)

#constru??o dos dataframes
## Carga
##
dataset <- loadDataFromFiles()
saveRDS(dataset,file = "c:/desenvolvimento/barometer/dataset5565L291C.rds")
dicionario<-dataDictionary()
saveRDS(dicionario,file = "c:/desenvolvimento/barometer/dicionario.rds")
territorios <- readxl::read_excel("Unidades da Federação, Mesorregiões, microrregiões e municípios 2010.xls",
    skip = 2)
"Micror-região"     "Nome_Microrregião" "Município"
names(territorios) <- c("cod_uf","nome_uf","cod_mesoreg","nome_mesoreg","cod_microreg","nome_microreg","cod_mun","nome_mun")
## Sele??o de atributos da dimens??o ambiental
DIM_AMBIENTAL <- dataset %>%
 filter(UF==15) %>%
 select(codigo=Codmun7,
        municipio = Munic?pio,
        T_LIXO, # % da popula??o que vive em domic?lios urbanos com servi?os de coleta de lixo. Alternativa: SNIS_CDI
        T_BANAGUA ,# %  da popula??o que vive em domic?lios com banheiro e ?gua encanada
        PINTERDRSAI, # % de interna??es por doen?as relacionadas ao saneamento ambiental inadequado
        AGUA_ESGOTO, # % de pessoas em domic?lios com abastecimento de ?gua e esgotamento sanitário inadequados
        PFOCOS, # Concentraa??o dos focos de calor
        PFLORA # Percentual de cobertura vegetal natural
        )

## Selea??o de atributos da dimens??o social
DIM_SOCIAL <- dataset %>%
 filter(UF==15) %>%
 mutate(
  POP_URB=pesourb/POP_TOT
 ) %>%
 select(
  codigo=Codmun7,
  municipio = Munic?pio,
  T_ANALF15M=T_ANALF15M, # Taxa de analfabetismo da popula??o de 15 anos ou mais de idade
  T_AGUA=T_AGUA, #  % da popula??o que vive em domic?lios com ?gua encanada
  T_ENV=T_ENV, # Taxa de envelhecimento)
  POP_URB, # % da popula??o em área urbana
  MORT5, # Mortalidade at? 5 anos de idade
)

## Selea??o de atributos da dimens??o Econômico
DIM_ECONOMICA <- dataset %>%
 filter(UF==15) %>%
 select(
  codigo=Codmun7,
  municipio = Munic?pio,
  REN_PIBPC_D, # Produto Interno Bruto per capita
  GINI, # ?ndice de Gini
  THEIL, # ?ndice de Theil-L
  THEILtrab, # ?ndice de Theil-L dos rendimentos do trabalho
  RAZDEP, # % da popula??o de menos de 15 anos e da popula??o de 65 anos e mais em rela??o ? popula??o de 15 a 64 anos
  CPR, # % de ocupados de 18 anos ou mais que s?o trabalhadores por conta pr?pria.
  P_FORMAL, # Grau de formaliza??o do trabalho das pessoas ocupadas
  T_ATIV, # Taxa de atividade das pessoas de 10 anos ou mais de idade
  RENOCUP, # Rendimento m?dio dos ocupados
  P_TRANSF, # % dos ocupados na ind?stria de transforma??o
  P_SIUP # % dos ocupados nos setores de servi?os industriais de utilidade p?blica
 ) %>%
 mutate(
  RENOCUP=RENOCUP/1000
 )

edm <-rbind(
  cbind("DIM_AMBIENTAL",colnames(DIM_AMBIENTAL)),
  cbind("DIM_ECONOMICA",colnames(DIM_ECONOMICA)),
  cbind("DIM_SOCIAL",colnames(DIM_SOCIAL))
)
edm <- edm[! edm[,2] %in% c("codigo", "municipio"),]
edm <- as.data.frame(edm)
edm <- merge(edm,dicionario,by.x = "V2", by.y = "acronym",all.x = T)

dataframe = merge(DIM_AMBIENTAL,merge(DIM_SOCIAL,DIM_ECONOMICA))

stats <- dataframe %>%
 gather(key = "V2",value = "valor",-codigo,-municipio) %>%
 select(-codigo,-municipio) %>%
 group_by(V2) %>%
 summarise_all( list(min=min, Q1=~quantile(., probs = 0.25),
                    median=median,mean=mean, Q3=~quantile(., probs = 0.75),
                    max=max))

edm<-merge(edm,stats)

rm(DIM_AMBIENTAL,DIM_ECONOMICA,DIM_SOCIAL,stats)



write.table(edm, file = "C:/desenvolvimento/barometer/Paper_Intervalo_EDM.csv",sep=';',dec = ',',
   row.names = F,col.names = T,fileEncoding = "UTF-16LE")

saveRDS(dataframe,file = "c:/desenvolvimento/barometer/dataframe143L24C.rds")
saveRDS(edm,file = "c:/desenvolvimento/barometer/edm.rds")
