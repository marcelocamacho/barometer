library(barometer)
library(tidyverse)
library(dplyr)

dataframe = readRDS(file = 'c:/desenvolvimento/barometer/dataframe143L24C.rds')

edm2 <- read.csv('c:/desenvolvimento/barometer/paper_edm2.csv',header = T,sep = ';')

edm <- select(edm2,-(dimension:max)) %>% rename(indicador=variable)

ebs=read.csv(file = 'c:/desenvolvimento/barometer/ebs.csv',sep = '|',dec = '.')
names(edm)[1]<-"INDICADOR"

territorios <- readxl::read_excel("Unidades da Federação, Mesorregiões, microrregiões e municípios 2010.xls",
    skip = 2)

names(territorios) <- c("cod_uf","nome_uf","cod_mesoreg","nome_mesoreg","cod_microreg","nome_microreg","cod_mun","nome_mun")

data <- dataframe %>%
    gather(key = "indicador",value ="valor" ,-codigo,-municipio) %>%
    select(-municipio) %>%
    mutate(codigo = as.character(codigo)) %>%
    spread(key = codigo,value = "valor")

data <- merge(data,edm2[,1:2],by.x = "indicador",by.y = "variable")

data<-data[,
            c(ncol(data),
              1:(ncol(data)-1))]
names(data)[1:2]<-c("DIMENSAO","INDICADOR")

bsData <- NULL

for (mun in names(data)[-c(1:2)]){
    print(mun)
    munVals <- eval(parse(text=paste0("data$`",mun,"`")))
    munData<-run(data$INDICADOR,munVals,mun)
    if(is.null(bsData)){
        bsData<-munData
    }

    bsData<-merge(bsData,munData)
}

bsData<-merge(data[,c(1:2)],bsData)

df <- bsData %>%
    gather(codigo, bs, -c(INDICADOR,DIMENSAO)) %>%
    inner_join(
        gather(dataframe,key=INDICADOR,value=valor,-c(codigo,municipio))
        ) %>%
    inner_join(.,territorios,by = c("codigo" = "cod_mun") ) %>%
    select(nome_mesoreg,nome_microreg,codigo,municipio,DIMENSAO,INDICADOR,valor,bs)
# Quantos municipios em cada meso-região?
df %>%
  group_by(nome_mesoreg) %>% summarise(n=n_distinct(municipio))

df$bs<-as.numeric(df$bs)
# quantidade de municípios amostrados
nMun=c(3,5,7,9)
# quantidade de amostragens aleatórias
nExec = 30
SMC<-NA
for (exec in 1:nExec){
  for (n in nMun){
    munAmostrados<-NULL
   for (reg in unique(df$nome_mesoreg)){
    if(is.null(munAmostrados)){
      munAmostrados <- territorios %>%
      filter(nome_mesoreg == reg) %>%
      select(cod_mun) %>%
        sample_n(.,n) %>%
        as.matrix()
    } else{

      munAmostrados <-c(munAmostrados,
                        territorios %>%
                          filter(nome_mesoreg == reg) %>%
                          select(cod_mun) %>%
                            sample_n(.,n) %>%
                            as.matrix()  )
    }
   }


    media_da_amostragem_n <- df %>%
        filter(codigo %in% munAmostrados) %>%
        select(-valor,-codigo,-municipio,-INDICADOR,-nome_microreg) %>%
        group_by(nome_mesoreg,DIMENSAO) %>%
        summarise(bs_mean = mean(bs)) %>%
        mutate(nMun=n,nExec=exec,munAmostrados=paste0(munAmostrados,collapse = ','))
    if(is.na(SMC)){
      SMC<-media_da_amostragem_n
    }else{
      SMC <- rbind(SMC,media_da_amostragem_n)
    }
  }
}

dados_grafico <- SMC %>%
  select(-nExec,-munAmostrados,-DIMENSAO) %>%
  group_by(nome_mesoreg,nMun) %>%
  summarise(bs=mean(bs_mean))

dados_grafico %>%
  filter(nome_mesoreg == "Nordeste Paraense") %>%
  ggplot(aes(x=nMun,y=bs)) + theme_minimal()+
  geom_point() +
  stat_smooth(method = "lm", col = "red")


dados_grafico %>%
  ggplot(aes(x=as.factor(nMun),y=bs,fill=as.factor(nome_mesoreg))) +
scale_fill_brewer(palette = "Set1") +
  theme_minimal()+
  geom_bar(stat="identity",position = "dodge" )

