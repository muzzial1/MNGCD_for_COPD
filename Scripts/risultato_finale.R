library(Matrix)
#library(clusterProfiler)
library(stringr)
library(dplyr)
args <- commandArgs(trailingOnly=TRUE)
print("AAAAAAAAAA")
print(args[1])#output_nome_multiplex
print("AAAAAAAAAA")
print(args[2])#gneset
gs<-as.character(args[2])
prima<-readRDS(file=paste0("/uhd/username/prova_nmi/",args[1],"/",sub("^[^_]*_", "", args[1]),"_10020_",gs,"/",gs,"_pperiter_",sub("^[^_]*_", "", args[1]),"_10020_",as.character(1),".rds"))
print("fatto1")
for(conta in 2:20){
  seconda<-readRDS(file=paste0("/uhd/username/prova_nmi/",args[1],"/",sub("^[^_]*_", "", args[1]),"_10020_",gs,"/",gs,"_pperiter_",sub("^[^_]*_", "", args[1]),"_10020_",as.character(conta),".rds"))
  #colnames(seconda)<-paste0("ST_",as.character(conta))
  prima<-cbind(prima,seconda)
}
print("fatto2")
totale<-prima
totalefv<-totale[apply(totale, 1, function(x) !all(is.na(x))),]

if(!is.null(dim(totalefv)))
{
	print("ecco")
	rates<-apply(totalefv,1,function(x)length(which(x!=0))/20)
	pmin<-apply(totalefv,1,function(x)min(x,na.rm = TRUE))
	pmax<-apply(totalefv,1,function(x)max(x,na.rm = TRUE))
	medie<-apply(totalefv,1,function(x)exp(mean(log(x),na.rm = TRUE)))
	ris<-cbind(rates,pmin,pmax,medie)
}
print(dim(t(t(totalefv))))
if(dim(t(t(totalefv)))[2]==1){
	print("Just one gene-set")
	totalefvunac<-totale[apply(totale, 1, function(x) !all(is.na(x))),1]
	print(names(totalefvunac))
	rates<-length(which(totalefv!=0))/20
	pmin<-min(totalefv,na.rm = TRUE)
	pmax<-max(totalefv,na.rm = TRUE)
	medie<-exp(mean(log(totalefv),na.rm = TRUE))
	#ris<-c(names(totalefvunac),rates,pmin,pmax,medie)
	ris<-c(rates,pmin,pmax,medie)
	#print(is.numeric(ris))
	#names(ris)<-c("set","rates","pmin","pmax","medie")
	ris<-t(ris)
	#rownames(ris)<-ris[1]
	rownames(ris)<-names(totalefvunac)
	colnames(ris)<-c("rates","pmin","pmax","medie")
	#ris <- subset(ris, select= -set)
	nomi<-dimnames(ris)
	#ris<-as.numeric(ris)
	print(nomi)
	#ris<-ris[,c("rates","pmin","pmax","medie")]
	#print("ecco")
	print(dim(ris))
}
print(ris)
saveRDS(object=ris,file=paste0("/uhd/username/prova_nmi/",args[1],"/final_result/",gs,"_finalres_",sub("^[^_]*_", "", args[1]),".rds"))

