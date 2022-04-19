options(warn=-1)


nivelBS <- function (bs_value){
  nivel = ifelse(bs_value<=20,"Insustentável",
                            ifelse(bs_value<=40,"P. Insustentável",
                                    ifelse(bs_value<=60,"Intermediário",
                                            ifelse(bs_value<=80,"P. Sustentável","Sustentável")
                                    )
                            )
                      )
  return(nivel)

}

convert<-function(x, type="numeric"){
  # converte colunas especificas de um dataframe
  numerico <- function(x){return(as.numeric(gsub(',','',x)))}
  df = apply(x, 1,numerico )
  df = as.data.frame(t(df))
  return(df)
}

discret<-function(x,crescent=TRUE){
  ##install.packages(arules)
  d_interval<-suppressMessages(arules::discretize(x,method = "interval",breaks = 5,infinity=T))
  limits<-attr(x = d_interval,which = 'discretized:breaks')
  ss=c("UNSUSTAINABLE","POTENTIALLY UNSUSTAINABLE","INTERMEDIARY","POTENTIALLY SUSTAINABLE","SUSTAINABLE")
  ord=unlist(ifelse(crescent==TRUE,list(ss[1:5]),list(ss[5:1])))
  df=data.frame(start=limits[-6],
                end=limits[-1],
                scales=ord)
  return(df)
}

between<-function(x,interval){
  interval = noquote(trimws(interval))
  inferior=as.numeric(gsub("\\[|\\]",'',strsplit(interval,split = ',')[[1]][1]))
  superior=as.numeric(gsub("\\[|\\]",'',strsplit(interval,split = ',')[[1]][2]))
  minimo=min(inferior,superior)
  maximo=max(inferior,superior)
  if(x>=minimo & x<=maximo){
    return(c(inferior,x,superior))
  } else{
    return(FALSE)
  }
}

compute_bs<-function(dma,dmx,dmp,bsa,bsp){
  return(((((dma-dmx)*(bsa-bsp))/(dma-dmp))*(-1))+bsa)
}

edm2eds<-function(x.indicator,x.factor,edm=edm){
##edm2eds -> Converte a Escala de Desenvolvimento Municipal na Escala do Barometro da sustentabilidade
# A função identifica o intervalo da EDM retornando os parametros adequados para a computação do bs
#
  df=NA
  for(i in x.indicator){
    a=which(edm$INDICADOR==i)
    x=which(x.indicator==i)
    interval = which(names(edm) %in% c("unsustainable","p.unsustainable","intermediary","p.sustainable","sustainable"))
    bias = which(names(edm) %in% "crescent")
    temp=NA
    for(j in edm[a,interval]){
      b=which(edm[a,interval]==j)+2
      #print(paste("i=",i,"a=",a,"j=",j,"b=",b,"edm[a,3:7]=",edm[a,3:7],"x.factor[a]",x.factor[x]))
      btw_test=isFALSE(between(as.numeric(x.factor[x]),j)[1])
     # print(paste(i,"-",b,btw_test,a,"=",i,b,"=",j,"btw_teste",btw_test))
      if(!btw_test){
        temp=c(i,
                      between(as.numeric(x.factor[x]),j),
                      names(edm[b]),
                      edm[a,bias])

      } else{
        if(b==7 & is.na(temp)){
          temp=c(i,
                 c(NA,x.factor[x],NA),
                 names(edm[b]),
                 edm[a,bias])
        }
      }
    }
    df=rbind(df,temp)
  }
  return(as.matrix(df[-1,],bycol=6))
}

bs<-function(x,classes=bsclasses){
## A função bs realiza a computação do Barometro da Sustentabilidade, retornando o vetor com os valores normalizados.
  status=x[5]
  dma=as.numeric(x[2]);dmx=as.numeric(x[3]);dmp=as.numeric(x[4])
  bsa=as.numeric(classes$start[classes$scales==status])
  bsp=as.numeric(classes$end[classes$scales==status])
  #print(paste(dma,dmx,dmp,bsa,bsp))
  df=compute_bs(dma,dmx,dmp,bsa,bsp)
  return(round(df,2))
}

## Vetor com a coluna que corresponde aos indicadores e o vetor contendo os respectivos dados do município
##
run<-function(x.indicator,x.factor,name=NULL){
  ## Input é dado como  dataframe$indicador, dataframe$nomedomunicipio
  ## Armazena nome dos argumentos como string
  ##
  if(is.null(name)){
    args=as.list(match.call(expand.dots=FALSE))
    var=unlist(args[[3]])[3]
  }else{
    var=name
  }
  result=NA
  df=edm2eds(x.indicator,x.factor,edm = edm)
  for(i in 1:nrow(df)){
    result=rbind(
      result,
      c(df[i,1],
        as.numeric(bs(df[i,],classes = ebs)))
      )
  }
  result=as.data.frame(result)
  names(result)<-c("INDICADOR",as.character(var))
  return(result)
}

fs_sd<-function(data,cutoff){
  df=sapply(data, sd)
  return(order(df,decreasing = T)[1:cutoff])
}
fs_cor<-function(data,cutoff,plot=FALSE){
  if(plot){
    corrplot::corrplot(cor(data), is.corr=FALSE,
                       tl.col="black", na.label=" ",
                       tl.cex = .6,sig.level = 0.05,)
  }
  return(order(colSums(abs(cor(data))),decreasing = T)[1:cutoff])
}
fs_pca<-function(data,cutoff,plot=FALSE){
  pca<-princomp(cor(data),cor =T)
  if(plot){
    screeplot(pca)
    #biplot(pca)
    fviz_contrib(pca,choice = "var")
  }
  return(order(prop.table(abs(pca$loadings)[,1]),decreasing = T)[1:cutoff])
}

