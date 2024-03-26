library(tidyr)
library(Matrix)
library(stringr)
library(igraph)

args<-commandArgs(trailingOnly = TRUE)
print(args[1])


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
  #farecbind dei due dataframe : comunità e nodo
  datiif<-list()
  for(n in 1:length(dataFiles)){datiif[[n]]<-cbind(datii[[n]],nodi[[n]])}
  #togliere 98, mettere una variabile che conta nella cartella di output il numero di files
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
  #da comando per shell si ricava infomappercons.txt
  write.table(dfg,file = paste0("/Users/username/Desktop/progetto/COPD/EpiHip001/network/matricipernetwork/matricipercontrasti/WP3/AllpcrposvsAllpcrneg_Bact/perinfomap/Allpcrneg_Bact/perconsenso/",n),row.names =FALSE,col.names = FALSE)
  
}

listalink_migliorata<-function(matrice,percorso){
  g  <- graph.adjacency(matrice,weighted=TRUE)
  dfg <- get.data.frame(g)
  write.table(dfg,file = percorso,row.names =FALSE,col.names = FALSE)
  
}

output_nspec<-args[2]
print(output_nspec)
nome_mux<-paste0(sub("^[^_]*_", "", output_nspec),".txt")#prendo


setwd("/uhd/username/prova_nmi/Scripts/")

set.seed(12)#lo setto di nuovo perchè ho dati in file
pescatos<-as.character(sample(1:1000000000,20))

partizioni<-leggi_partizioni(paste0("/uhd/username/prova_nmi/",output_nspec,"/outputinfomap/",output_nspec,"_",as.character(args[1]),"/"))
num_nodi<-2657
psb<-lapply(partizioni, function(x)fuori_banali(x,quali_banali(x,num_nodi)))
setwd(paste0("/uhd/username/prova_nmi/",output_nspec,"/perconsenso/",output_nspec,"_",as.character(args[1]),"/"))
system("touch grafoc.txt")
system("mkdir outputperconsenso")
for(iterazione in 1:20){
    print(iterazione)
    if(iterazione>1){
      rm(matriciperconsenso)}
    matriciperconsenso<-lapply(psb,matricedafile,nodi=num_nodi)
    print("matrici in lista calcolate")
    D2<-somma_l(matriciperconsenso)
    print("ora consenso")
    D3<-consensoconfiltro(D2)
    print("consenso temporaneo calcolato")
    if(length(unique(matriciperconsenso))==1){
    	print("esco")
	break}
    system(">grafoc.txt")
    listalink_migliorata(D3,"grafoc.txt")
    system("rm outputperconsenso/*")
    if(iterazione==1){
      print("pesco per iterazioni")
      set.seed(3546)#lo setto di nuovo perchè ho dati in file
      pescatos<-as.character(sample(1:1000000000,20))}#ho messo numero più alto
    genero_partizioni_perconsenso(filebash = "/uhd/username/prova_nmi/Scripts/generoperconsenso_cal.sh",pescato = pescatos[iterazione],multiplex = paste0(output_nspec,"_",as.character(args[1])),output_nspec = output_nspec)#nome del multiplex è il secondo argomento di shell script
    #rm(partizioni)
    #cambiare ultime 4 righe come si è fatto sopra
    partizioni<-leggi_partizioni(paste0("/uhd/username/prova_nmi/",output_nspec,"/perconsenso/",paste0(output_nspec,"_",as.character(args[1])),"/outputperconsenso/"))
    psb<-lapply(partizioni, function(x)fuori_banali(x,quali_banali(x,num_nodi)))
  }
D3<-(D3-Diagonal(num_nodi,diag(D3))+Diagonal(num_nodi,1))
D3l<-list(D3)
save(D3l,file=paste0("/uhd/username/prova_nmi/",output_nspec,"/consensi/",paste0(output_nspec,"_",as.character(args[1])),".RData"))


