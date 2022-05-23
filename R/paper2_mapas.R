# Geração de mapas
 require(maptools)
 require(RColorBrewer)

 # Selecionamos algumas cores de uma paleta de cores do pacote RColorBrewer
 ebs$scales<-factor(x=c("unsustainable","p.unsustainable","intermediary","p.sustainable","sustainable"),levels=c("unsustainable","p.unsustainable","intermediary","p.sustainable","sustainable"))
 paletaDeCores = brewer.pal(5, 'RdYlGn')
 coresDasCategorias = data.frame(nivel=levels(ebs$scales), Cores=paletaDeCores)
 # Mesoregião nordeste
 {
  df_ambiental<-df %>%
   select(-valor,-INDICADOR) %>%
   group_by(nome_mesoreg,nome_microreg,codigo,municipio,DIMENSAO) %>%
   summarise(bs=mean(bs)) %>% filter(DIMENSAO=='DIM_AMBIENTAL')
  df_ambiental$nivel<-as.factor(nivelBS(df_ambiental$bs))

  mapaUF = readShapePoly("./inst/shapefile_pa_municipios/2000/municipios/15MU500G.shp")

  mapaData = attr(mapaUF, 'data')
  mapaData$Index = row.names(mapaData)
  mapaUF_df<-fortify(mapaUF)
  mapaUF_df$Index<-as.factor(mapaUF_df$id)

  df_ambiental = merge(df_ambiental, coresDasCategorias)
  mapaData_ambiental=merge(mapaData, df_ambiental,by.x="CODIGO",by.y="codigo")
  attr(mapaUF, 'data')= mapaData_ambiental

  centroids <- setNames(do.call("rbind.data.frame", by(mapaUF_df, mapaUF_df$Index, function(x) {
   Polygon(x[c('long', 'lat')])@labpt
  })), c('long', 'lat'))
  centroids$factors <- levels(mapaUF_df$Index)

  # Configurando tela (reduzindo as margens da figura)
  parDefault = par(no.readonly = T)
  layout(matrix(c(1,2),nrow=2),widths= c(1,1), heights=c(4,1))
  par (mar=c(0,0,0,0))
  plot(mapaUF, col=as.character(mapaData_ambiental$Cores))
  text(centroids$long,centroids$lat,centroids$factors,cex = 0.7,pos = 4,col = "black", offset=-0.2)
  plot(1,1,pch=NA, axes=F)
  legend(x='center', legend=rev(coresDasCategorias$nivel),
         box.lty=0, fill=rev(paletaDeCores),cex=.8, ncol=2,
         title='Mapa dos municípios paraenses segundo a dimensão ambiental')
}
