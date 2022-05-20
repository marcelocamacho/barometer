#library(barometer)
library(tidyverse)
library(dplyr)
source("R/barometer.R")

# Load
{
dataframe = readRDS(file = './dataframe143L24C.rds')
edm_with_statistic <- read.csv('./paper_edm2.csv',header = T,sep = ';',fileEncoding = "UTF-8")
edm <- select(edm_with_statistic,-(indicated:UNIDADE)) %>% rename(indicador=variable)
names(edm)[1]<-"INDICADOR"

ebs=read.csv(file = './ebs.csv',sep = '|',dec = '.')

territorios <- readxl::read_excel("./Unidades da Federação, Mesorregiões, microrregiões e municípios 2010.xls",
    skip = 2)

names(territorios) <- c("cod_uf","nome_uf","cod_mesoreg","nome_mesoreg","cod_microreg","nome_microreg","cod_mun","nome_mun")

} #End Load

# Transform
{
  data <- dataframe %>%
    gather(key = "indicador",value ="valor" ,-codigo,-municipio) %>%
    select(-municipio) %>%
    mutate(codigo = as.character(codigo)) %>%
    spread(key = codigo,value = "valor")

  data <- merge(data,edm[,1:2],by.x = "indicador",by.y = "INDICADOR")

  data<-data[,
            c(ncol(data),
              1:(ncol(data)-1))]
  names(data)[1:2]<-c("DIMENSAO","INDICADOR")

} #End Tranform

# Barometer Process
{

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

  rm(data,dataframe,edm,edm_with_statistic)

  df$bs<-as.numeric(df$bs)

} #End Barometer Process

## ALTERAÇÃO NA QUANTIDADE DE REGIÕES
## grupo 1 (baixo amazonas e marajo),
## grupo 2  (metropolitana e nordeste) e
## grupo 3 (sudeste e sudoeste)
##
#df$nome_mesoreg[df$nome_mesoreg %in% c('Baixo Amazonas','Marajó')] <- 'Grupo 1'
#df$nome_mesoreg[df$nome_mesoreg %in% c('Metropolitana de Belém','Nordeste Paraense')] <- 'Grupo 2'
#df$nome_mesoreg[df$nome_mesoreg %in% c('Sudeste Paraense','Sudoeste Paraense')] <- 'Grupo 3'


## ANÁLISES
{
  # Quantos municipios em cada meso-região?

df %>%
  group_by(nome_mesoreg) %>% summarise(municipios=n_distinct(municipio))

# Qual a situação da Sustentabilidade em cada região?
df %>%
  select(-valor,-INDICADOR) %>%
  group_by(nome_mesoreg,nome_microreg,codigo,municipio,DIMENSAO) %>%
  summarise(bs=mean(bs)) %>%
    group_by(nome_mesoreg,nome_microreg,codigo,municipio) %>%
    summarise(bs=mean(bs)) %>%
      group_by(nome_mesoreg,nome_microreg) %>%
      summarise(bs=mean(bs),municipios=n_distinct(municipio)) %>%
        group_by(nome_mesoreg) %>%
        summarise(municipios=sum(municipios),
                  bs=mean(bs)) %>%
          mutate(nivel=nivelBS(bs))

# Quantos e como os municípios são classificados por região?
df %>%
  select(-valor,-INDICADOR,-nome_microreg,-municipio) %>%
  group_by(nome_mesoreg,codigo,DIMENSAO) %>%
  summarise(bs=mean(bs)) %>%
    group_by(nome_mesoreg,codigo) %>%
    summarise(bs=mean(bs)) %>%
    mutate(nivel=nivelBS(bs)) %>%
    select(nome_mesoreg,nivel) %>%
    table(.)

# Tabela atributo x classificação
df %>%
 select(-valor,-nome_microreg,-municipio,-DIMENSAO) %>%
 group_by(codigo,INDICADOR) %>%
 summarise(bs=mean(bs),nivel=nivelBS(mean(bs))) %>% ungroup(.) %>%
 select(INDICADOR,nivel) %>% table()


# Correlação entre as regiões (KNN, Pearson)
df %>%
 select(-valor,-INDICADOR,-nome_microreg,-municipio) %>%
 group_by(nome_mesoreg,codigo,DIMENSAO) %>%
 summarise(bs=mean(bs)) %>%
 group_by(nome_mesoreg,DIMENSAO) %>%
 summarise(bs=mean(bs)) %>% ungroup(.) %>%
 spread(.,nome_mesoreg,bs) %>%
 as.data.frame(row.names = DIMENSAO) %>%
 select(-DIMENSAO) %>% cor()


}

# Geração de mapas
# https://jtr13.github.io/cc19/plotting-maps-with-r-an-example-based-tutorial.html
# https://slcladal.github.io/maps.html
# https://www.geeksforgeeks.org/adding-labels-to-points-plotted-on-world-map-in-r/
#https://stackoverflow.com/questions/57867991/how-to-annotate-r-map-with-text-and-points
require(XML)
require(RCurl)
require(maptools)
require(RColorBrewer)
require(ggplot2)

mapaUF = readShapePoly("15MU500G.shp")
#mapaMun = rgdal::readOGR("15MU500G.shp")

paletaDeCores = brewer.pal(9, 'OrRd')
mapaData = attr(mapaUF, 'data')
mapaUF_df<-fortify(mapaUF)
mapaUF_df$id<-as.factor(mapaUF_df$id)
mapaData$Index = row.names(mapaData)

df_ambiental<-df %>%
  select(-valor,-INDICADOR) %>%
  group_by(nome_mesoreg,nome_microreg,codigo,municipio,DIMENSAO) %>%
  summarise(bs=mean(bs)) %>% filter(DIMENSAO=='DIM_AMBIENTAL')

df_ambiental$nivel<-as.factor(nivelBS(df_ambiental$bs))


temp <- mapaUF
a <- temp@data
a <- a %>% mutate(identifier = strtoi(rownames(a)) + 1)
b <- sp::merge(a, df_ambiental, by.x = "CODIGO", by.y="codigo", all.x= TRUE)
b <- b[order(b$identifier),]
row.names(b) <- 0:157
temp@data <- b
rm(a,b)

# Selecionamos algumas cores de uma paleta de cores do pacote RColorBrewer
paletaDeCores = brewer.pal(9, 'OrRd')
paletaDeCores = paletaDeCores[-c(1,3,6,9)]
coresDasCategorias = data.frame(nivel=levels(as.factor(ebs$scales)), Cores=paletaDeCores)

df_ambiental = merge(df_ambiental, coresDasCategorias)

mapaData_ambiental=merge(mapaData, df_ambiental,by.x="CODIGO",by.y="codigo")
attr(mapaUF, 'data')= mapaData_ambiental






centroids <- setNames(do.call("rbind.data.frame", by(mapaUF_df, mapaUF_df$id, function(x) {
  Polygon(x[c('long', 'lat')])@labpt
 })), c('long', 'lat'))
centroids$factors <- levels(mapaUF_df$id)



plot(temp, col=as.character(mapaData_ambiental$Cores))
text(centroids$long,centroids$lat,centroids$factors,cex = 0.7,pos = 4,col = "black", offset=-0.2)
#
# map <- ggplot(data = mapaUF_df, aes(x = lat, y = long)) +
#   geom_polygon(data = mapaUF) +
#   with(centroids, annotate(geom="text", x = lat, y = long,
#                            label=factors, size=3)) +
#   scale_fill_gradient(low="white", high="darkgreen") +
#   ggtitle("Vehicle-Related Crimes by Milwaukee Districts in 2018") +
#   theme(plot.title = element_text(hjust = 0.5))



# Configurando tela (reduzindo as margens da figura)
parDefault = par(no.readonly = T)
layout(matrix(c(1,2),nrow=2),widths= c(1,1), heights=c(4,1))
par (mar=c(0,0,0,0))

# Colar o código do mapa aqui com a legenda

plot(1,1,pch=NA, axes=F)
legend(x='center', legend=rev(levels(mapaData_ambiental$nivel)),
 box.lty=0, fill=rev(paletaDeCores),cex=.8, ncol=2,
 title='Mapa dos municípios paraenses segundo a dimensão ambiental')




# Monte Carlo Process
{
  # quantidade de municípios amostrados
  nMun=c(2,4,6,8,10)
 # nMun=c(2,4,6,8,10,12,14,16,18,20)
  # quantidade de amostragens aleatórias
  nExec = 1
  SMC<-NA
  munAmostrados<-NULL
  for (exec in 1:nExec){
    for (n in nMun){
     for (reg in unique(df$nome_mesoreg)){
       munAmostrados <- df %>% select(nome_mesoreg,codigo) %>% distinct() %>%
         filter(nome_mesoreg == reg) %>% select(codigo) %>% sample_n(.,n) %>% as.matrix()

      media_da_amostragem_n <- df %>%
          filter(codigo %in% munAmostrados) %>%
          select(-valor,-codigo,-municipio,-INDICADOR,-nome_microreg) %>%
          group_by(nome_mesoreg,DIMENSAO) %>%
          summarise(bs_mean = mean(bs),.groups = 'drop') %>%
          mutate(nMun=n,nExec=exec,munAmostrados=paste0(munAmostrados,collapse = ','))
     cat(
       sprintf("nExec=%d, nMun=%d, reg=%s\n",exec,nMun,reg)
       )
      if(is.na(SMC)){
        SMC<-media_da_amostragem_n
      }else{
        SMC <- rbind(SMC,media_da_amostragem_n)
      }
     }
    }
  }
  rm(munAmostrados,munData,media_da_amostragem_n)
} # End Monte Carlo Process
# Data visualization
{
  dados_grafico <- SMC %>%
    select(-nExec,-munAmostrados,-DIMENSAO) %>%
    group_by(nome_mesoreg,nMun) %>%
    summarise(bs=mean(bs_mean))

 BS_medio_por_regiao <- dados_grafico %>%
    ggplot(aes(x=as.factor(nMun),y=bs,fill=as.factor(nome_mesoreg))) +
  scale_fill_brewer(palette = "Set1") +
    theme_minimal()+
    labs(fill = "Mesoregião",title = "Barômetro da Sustentabilidade médio por região",caption = "Média do BS por mesoregião, de acordo com o tamanho da amostra ( Nº de municípios)") + xlab("Nº de municípios amostrados") + ylab("Valor do BS (média)")+
    geom_bar(stat="identity",position = "dodge" )
BS_medio_por_regiao

ifelse( !require(ggpubr) ,install.packages("ggpubr"),library(ggpubr))

 for (reg in unique(dados_grafico$nome_mesoreg)){
   varName = paste("plotlm",str_replace(reg,' ','_'),sep = '_')
  assign(varName,
         dados_grafico %>%
          filter(nome_mesoreg == reg) %>%
          ggplot(aes(x=nMun,y=bs)) + theme_minimal()+
          geom_point() +
           stat_regline_equation(aes(label =
                                      paste(..eq.label..,..adj.rr.label..,sep = "*plain(\",\")~~")),
                                label.x = 15, label.y = 20,size=3) +
          geom_smooth(method = "lm", col = "red") +

           theme_classic()
         )
 }
ifelse( !require(cowplot) ,install.packages("cowplot"),library(cowplot))

  Tendencia_por_mesoregiao <- do.call(ggarrange,list(ncol=1,labels="AUTO",plotlist=mget(ls(pattern = 'plotlm_'))))
  annotate_figure(Tendencia_por_mesoregiao,
                  top = text_grob("Visualizing Tooth Growth", color = "red", face = "bold", size = 14),
                  bottom = text_grob("Data source: \n ToothGrowth data set", color = "blue", face = "italic", size = 10))

#c("Nordeste","Sudeste","Marajó","Baixo Amazonas","Sudeste","Região Metropolitana")
} #End Data visualization
# Salvando os dados
{
 write.table(x = SMC, file = "BS_SMC_3Grupos20Mun_310322.csv",sep = ';',dec = ',',row.names = F)
 ggsave("BS_medio_por_3grupos20Mun_310322.png", plot = BS_medio_por_regiao)
 ggsave("Tendencia_por_3grupos20Mun_310322.png", plot = Tendencia_por_mesoregiao)
}
