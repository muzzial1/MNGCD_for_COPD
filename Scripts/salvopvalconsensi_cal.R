library(Matrix)
library(clusterProfiler)
library(stringr)
library(dplyr)
args <- commandArgs(trailingOnly=TRUE)
print("AAAAAAAAAA")
print(args[2])
print("AAAAAAAAAA")
print(args[3])
gs<-as.character(args[3])
if(!(gs %in% c("h","c2","c3","c5","c7"))){
	h<-readRDS(paste0("/uhd/username/prova_nmi/genesets/2019_",as.character(args[3]),".rds"))}
if(gs %in% c("h","c2","c3","c5","c7")){
	h<-read.gmt(paste0("/uhd/username/prova_nmi/genesets/",as.character(args[3]),".all.v6.1.entrez.gmt.txt"))}


  load(file =paste0("/uhd/username/prova_nmi/",args[2],"/",sub("^[^_]*_", "", args[2]),"_10020_",gs,"/",gs,"_comarr_mux_",sub("^[^_]*_", "", args[2]),"_10020_",as.character(args[1]),".RData"))
  xlperp<-lapply(xl,function(x)if(is.null(x))return(data_frame(ID=0,geneID=0))else return(x@result[,c("ID","p.adjust")]))
  numcat<-length(unique(h$ont))
  categorie<-as.vector(unique(h$ont))
s<-lapply(categorie,function(z)unlist(lapply(xlperp,function(x){if(rownames(x)==1||dim(x)[1]==0||(!z %in% rownames(x))){return(NA)}else{return(x[z,"p.adjust"])}})))
sunita<-do.call(rbind,s) 
  padjust<-Matrix(data = sunita,sparse = TRUE)
  rownames(padjust)<-categorie
saveRDS(padjust,file=paste0("/uhd/username/prova_nmi/",args[2],"/",sub("^[^_]*_", "", args[2]),"_10020_",gs,"/",gs,"_pfrom_",sub("^[^_]*_", "", args[2]),"_10020_",as.character(args[1]),".rds"))

  seconda<-t(t(apply(padjust,1,min,na.rm=TRUE)))
  seconda[is.infinite(seconda)]<-NA 
saveRDS(seconda,file=paste0("/uhd/username/prova_nmi/",args[2],"/",sub("^[^_]*_", "", args[2]),"_10020_",gs,"/",gs,"_pperiter_",sub("^[^_]*_", "", args[2]),"_10020_",as.character(args[1]),".rds"))

