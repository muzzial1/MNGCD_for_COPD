args <- commandArgs(trailingOnly=TRUE)
print(args[1])
print("AAAAAA")
print(args[2])
library(tidyr)
library(Matrix)
library(stringr)
library(igraph)
#library(parallel)
#library(foreach)
#library(doParallel)


 
genero_partizioni<-function(filebash,pescato){
  system(paste(paste0("./",filebash),pescato))}

genero_partizioni_v2<-function(filebash,pescato,output_nspec){
  system(paste(paste0("./",filebash),pescato,output_nspec))}

genero_partizioni_v3<-function(filebash,pescato,multiplex,output_nspec,out2){
  system(paste(paste0("./",filebash),pescato,multiplex,out2,output_nspec))}

genero_partizioni_perconsenso<-function(filebash,pescato,multiplex,output_nspec){system(paste(filebash,pescato,multiplex,output_nspec))}

leggi_partizioni<-function(percorso){
  dataFiles <- lapply(Sys.glob(paste0(percorso,"/*.tree")), read.table)
  dati<-lapply(dataFiles,function(x)cbind(str_split_fixed(x$V1, ":(?=[^:]+$)",n=2),x$V3))
  dati<-lapply(dati,function(x)as.data.frame(x, stringsAsFactors=FALSE))
  nodi<-lapply(dati,function(x)x$V3)
  nodi<-lapply(nodi,function(x)as.data.frame(x, stringsAsFactors= FALSE))
  datii<-lapply(dati,function(x)str_split_fixed(x$V1, "(?=[^:]+$)",n=2))
  datii<-lapply(datii,function(x)as.data.frame(x, stringsAsFactors=FALSE))
  datii<-lapply(datii,function(x)x$V2)
  datii<-lapply(datii,function(x)as.data.frame(x, stringsAsFactors=FALSE))
  datiif<-list()
  for(n in 1:length(dataFiles)){datiif[[n]]<-cbind(datii[[n]],nodi[[n]])}
  datiif<-lapply(datiif,function(x){colnames(x)<-c("indice","V3")
  return(x)})
  datiif<-lapply(datiif,function(x)x[!duplicated(x),])
  return(datiif)}

overlap_cisono<-function(dframe,numnodi){
  return(dim(dframe)[1]!=numnodi)
}

banali_cisono<-function(dframe,numnodi){
  risposta=FALSE
  for(com in unique((dframe$indice))){
    if(dim(dframe[dframe$indice==com,])[1]==numnodi){
      risposta=TRUE
      break()
    }

  }
  return(risposta)
}

quali_banali<-function(dframe,numnodi){
  banali<-vector()
  for(com in unique((dframe$indice))){
    if(dim(dframe[dframe$indice==com,])[1]==numnodi){
      banali<-c(banali,com)
    }

  }
  return(banali)
}

fuori_banali<-function(dframe,qualibanali){
  indici<-!(dframe$indice %in% qualibanali)
  return(dframe[indici,])
}

vecchiomatricedafile<-function(dframe){
  comunit<-unique(dframe$indice)
  m<-Matrix(0,dim(dframe)[1],dim(dframe)[1],sparse=TRUE)
  for(n in comunit){
    df1<-dframe[dframe$indice==n,]
    a<-do.call(expand.grid,rep(list(df1$V3),2))
    if(is.factor(a$Var1) && is.factor(a$Var2)){
      a$Var1 <- as.numeric(as.character(a$Var1))
      a$Var2 <- as.numeric(as.character(a$Var2))}
    m[as.matrix(a)]<-1
  }
  return(m)
}

matricedafile<-function(dframe,nodi){
  comunit<-unique(dframe$indice)
  m<-Matrix(0,nodi,nodi,sparse=TRUE)
  for(n in comunit){
    df1<-dframe[dframe$indice==n,]
    a<-do.call(expand.grid,rep(list(df1$V3),2))
    if(is.factor(a$Var1) && is.factor(a$Var2)){
      a$Var1 <- as.numeric(as.character(a$Var1))
      a$Var2 <- as.numeric(as.character(a$Var2))}
    m[as.matrix(a)]<-1
  }
  return(m)
}

matricedafilepar<-function(dframe,nodi){
  comunit<-unique(dframe$indice)
  m<-Matrix(0,nodi,nodi,sparse=TRUE)
  foreach(n = comunit, .packages= "Matrix") %do% {
    df1<-dframe[dframe$indice==n,]
    a<-do.call(expand.grid,rep(list(df1$V3),2))
    if(is.factor(a$Var1) && is.factor(a$Var2)){
      a$Var1 <- as.numeric(as.character(a$Var1))
      a$Var2 <- as.numeric(as.character(a$Var2))}
    m[as.matrix(a)]<-1
  }
  return(m)
}

somma_l<-function(listam){
  D<-Reduce('+',listam)
  D<-(D/length(listam))
  return(D)
}

consensoconfiltro<- function(D2){
  D3<-D2
  D3[D3<0.5]<-0
  D2a<-D2-diag(diag(D2))
  i<-which(colSums(D3)==1)
  if(length(i)!=0){
    D2b<-D2a[,i]
    D2b<-Matrix(D2b,sparse = TRUE)
    m2<-apply(D2b,2,max)
    indicimax<-vector()
    if(length(i)>1){
      for(n in 1:length(i)){
        if(length(which(D2b[,n]==m2[n]))==1){indicimax<-c(indicimax,which(D2b[,n]==m2[n]))}
        else{indicimax<-c(indicimax,sample(x=which(D2b[,n]==m2[n]),size=1))}}
      #ora ho indicimax che mi dà indici dei nodi con cui il nodo, isolato in D3 ha link più forte
      for(m in 1:length(i)){
        D3[indicimax[m],i[m]]<-m2[m]
        D3[i[m],indicimax[m]]<-m2[m]
      }
    }
    if(length(i)==1){
      if(length(which(D2b[,1]==m2))==1){indicimax<-c(indicimax,which(D2b[,1]==m2))}
      else{indicimax<-c(indicimax,sample(x=which(D2b[,1]==m2),size=1))}
      D3[indicimax,i]<-m2
      D3[i,indicimax]<-m2
    }
  }
  return(D3)
}

listalink<-function(matrice,nomefile){
  n <- nomefile
  g  <- graph.adjacency(matrice,weighted=TRUE)
  dfg <- get.data.frame(g)
  write.table(dfg,file = paste0("/uhd/username/perinfomap/Allpcrneg_Bact/perconsenso/",n),row.names =FALSE,col.names = FALSE)

}

listalink_migliorata<-function(matrice,percorso){
  g  <- graph.adjacency(matrice,weighted=TRUE)
  dfg <- get.data.frame(g)
  write.table(dfg,file = percorso,row.names =FALSE,col.names = FALSE)

}

output_nspec<-args[2]
nome_mux<-paste0(sub("^[^_]*_", "", output_nspec),".txt")#prendo muxi
print(nome_mux)

setwd("./Scripts/")

a<-paste0(output_nspec,"_",as.character(args[1]))
genero_partizioni_v3(filebash = "/uhd/username/prova_nmi/Scripts/myscript_varifile_copy_cal.sh",pescato = args[1],multiplex = nome_mux,out2 = a,output_nspec = output_nspec)
