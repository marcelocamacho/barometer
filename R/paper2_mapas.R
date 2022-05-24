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
  mapaData = attr(mapaUF, 'data')
  mapaData$Index = row.names(mapaData)

  #Calcular centroids
  mapaUF_df<-fortify(mapaUF)
  mapaUF_df<-unique(mapaUF_df)
  mapaUF_df$Index<-as.factor(mapaUF_df$id)
  centroids <- setNames(do.call("rbind.data.frame", by(mapaUF_df, mapaUF_df$Index, function(x) {
   Polygon(x[c('long', 'lat')])@labpt
  })), c('long', 'lat'))
  centroids<-unique(centroids)
  centroids$factors <- levels(mapaUF_df$Index)

  rm(mapaUF_df)

  trueCentroids = rgeos::gCentroid(mapaUF,byid=TRUE)
  trueCentroids = fortify(trueCentroids)
  trueCentroids$id = row.names(trueCentroids)


  plot(mapaUF)
  text(trueCentroids,cex = 0.7)
  points(coordinates(mapaUF),pch=1)
  points(trueCentroids,pch=2)


  df_nordeste = merge(df_nordeste, coresDasCategorias)
  mapaData_nordeste=merge(mapaData, df_nordeste,by.x="CODIGO",by.y="codigo")
  attr(mapaUF, 'data')= df_nordeste

  # Configurando tela (reduzindo as margens da figura)
  parDefault = par(no.readonly = T)
  layout(matrix(c(1,2),nrow=2),widths= c(1,1), heights=c(4,1))
  par (mar=c(0,0,0,0))
  plot(mapaUF, col=as.character(mapaData_nordeste$Cores))
  text(centroids$long,centroids$lat,centroids$factors,cex = 0.7,pos = 4,col = "black", offset=-0.2)
  plot(1,1,pch=NA, axes=F)
  legend(x='center', legend=rev(coresDasCategorias$nivel),
         box.lty=0, fill=rev(paletaDeCores),cex=.8, ncol=2,
         title='Classificação da sustentabilidade na região nordeste Paraense')
}

