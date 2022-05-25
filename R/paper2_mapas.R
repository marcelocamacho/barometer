# Geração de mapas
 require(maptools)
 require(RColorBrewer)
 require(rgeos)

 # Selecionamos algumas cores de uma paleta de cores do pacote RColorBrewer
 ebs$scales<-factor(x=c("unsustainable","p.unsustainable","intermediary","p.sustainable","sustainable"),levels=c("unsustainable","p.unsustainable","intermediary","p.sustainable","sustainable"))
 paletaDeCores = brewer.pal(5, 'RdYlGn')
 coresDasCategorias = data.frame(nivel=levels(ebs$scales), Cores=paletaDeCores)
 # Mesoregião nordeste
 {
  df_nordeste<-df %>%
   select(-valor,-INDICADOR,-DIMENSAO) %>%
   filter(nome_mesoreg =='Nordeste Paraense') %>%
   group_by(nome_mesoreg,nome_microreg,codigo,municipio) %>%
   summarise(bs=mean(bs))
  df_nordeste$nivel<-as.factor(nivelBS(df_nordeste$bs))

  mapaUF = readShapePoly("./inst/shapefile_pa_municipios/2000/municipios/15MU500G.shp")
  mapaUF = mapaUF[mapaUF@data$CODIGO %in% df_nordeste$codigo,]
  mapaUF = mapaUF[mapaUF@data$SEDE == TRUE,]
  mapaData = attr(mapaUF, 'data')
  mapaData$Index = row.names(mapaData)

  #Calcular centroids
  mapaUF_df<-fortify(mapaUF)
  mapaUF_df$Index<-as.factor(mapaUF_df$id)

  centroids <- setNames(do.call("rbind.data.frame", by(mapaUF_df, mapaUF_df$Index, function(x) {
   Polygon(x[c('long', 'lat')])@labpt
  })), c('long', 'lat'))
  centroids$Index <- levels(mapaUF_df$Index)
  centroids$factors <- 1:nrow(centroids)
  rm(mapaUF_df)
  df_nordeste = merge(df_nordeste, coresDasCategorias)
  mapaData_nordeste=merge(mapaData, df_nordeste,by.x="CODIGO",by.y="codigo")
  attr(mapaUF, 'data')= df_nordeste

  # Configurando tela (reduzindo as margens da figura)
  parDefault = par(no.readonly = T)
  layout(matrix(c(1,2),nrow=2),widths= c(1,1), heights=c(5,1))
  par (mar=c(0,0,0,0))
  plot(mapaUF, col=as.character(mapaData_nordeste$Cores))
  text(centroids$long,centroids$lat,centroids$factors,cex = 0.6,col = "black",pos = 1,offset = .15)
  #text(mapaUF@data$LONGITUDES,mapaUF@data$LATITUDESE,row.names(mapaUF@data),cex = 0.6,col = "black",pos = 1,offset = 0.2)
  plot(1,1,pch=NA, axes=F)
  par (mar=c(0,0,0,0))
  legend(x='center', legend=rev(coresDasCategorias$nivel),
         box.lty=0, fill=rev(paletaDeCores),cex=.8, ncol=2,
         title='Classificação da sustentabilidade na região nordeste Paraense')
 #mapaData[mapaData$Index == centroids[centroids$factors==21,]$Index,]
 rm(df_nordeste,mapaData_nordeste,mapaUF_df,parDefault,trueCentroids,centroids,mapaUF,munData,mapaData)
 }

 # Mesoregião Sudeste
 {
  df_sudeste<-df %>%
   select(-valor,-INDICADOR,-DIMENSAO) %>%
   filter(nome_mesoreg =="Sudeste Paraense") %>%
   group_by(nome_mesoreg,nome_microreg,codigo,municipio) %>%
   summarise(bs=mean(bs))
  df_sudeste$nivel<-as.factor(nivelBS(df_sudeste$bs))

  mapaUF = readShapePoly("./inst/shapefile_pa_municipios/2000/municipios/15MU500G.shp")
  mapaUF = mapaUF[mapaUF@data$CODIGO %in% df_sudeste$codigo,]
  mapaUF = mapaUF[mapaUF@data$SEDE == TRUE,]
  mapaData = attr(mapaUF, 'data')
  mapaData$Index = row.names(mapaData)

  #Calcular centroids
  mapaUF_df<-fortify(mapaUF)
  mapaUF_df$Index<-as.factor(mapaUF_df$id)

  centroids <- setNames(do.call("rbind.data.frame", by(mapaUF_df, mapaUF_df$Index, function(x) {
   Polygon(x[c('long', 'lat')])@labpt
  })), c('long', 'lat'))
  centroids$Index <- levels(mapaUF_df$Index)
  centroids$factors <- 1:nrow(centroids)
  rm(mapaUF_df)
  df_sudeste = merge(df_sudeste, coresDasCategorias)
  mapaData_sudeste=merge(mapaData, df_sudeste,by.x="CODIGO",by.y="codigo")
  attr(mapaUF, 'data')= df_sudeste

  # Configurando tela (reduzindo as margens da figura)
  parDefault = par(no.readonly = T)
  layout(matrix(c(1,2),nrow=2),widths= c(1,1), heights=c(5,1))
  par (mar=c(0,0,0,0))
  plot(mapaUF, col=as.character(mapaData_sudeste$Cores))
  text(centroids$long,centroids$lat,centroids$factors,cex = 0.5,col = "black")
  #text(mapaUF@data$LONGITUDES,mapaUF@data$LATITUDESE,row.names(mapaUF@data),cex = 0.6,col = "black",pos = 1,offset = 0.2)
  plot(1,1,pch=NA, axes=F)
  par (mar=c(0,0,0,0))
  legend(x='center', legend=rev(coresDasCategorias$nivel),
         box.lty=0, fill=rev(paletaDeCores),cex=.8, ncol=2,
         title='Classificação da sustentabilidade na região sudeste Paraense')
 #mapaData[mapaData$Index == centroids[centroids$factors==21,]$Index,]
 }

