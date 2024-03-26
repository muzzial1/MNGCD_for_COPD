library(Matrix)
library(clusterProfiler)
args <- commandArgs(trailingOnly=TRUE)
print("AAAAAAAAAA")
print(args[2])
print("AAAAAAAAAA")
print(args[3])
gs<-as.character(args[3])
load("/uhd/username/prova_nmi/Scripts/genilabel/translate_20180607_WK4_b_2_Labels_PCR_HivsMC_I_MC.RData")
nomigeni<-nomigenil[[1]]#sono identici nei 2 contrasti
if(args[3]=="BTM"||args[3]=="TF"||args[3]=="mirna"){
	h<-readRDS(paste0("/uhd/username/prova_nmi/genesets/2019_",as.character(args[3]),".rds"))}
if(args[3] %in% c("h","c3","c5","c7","c2","c1")){
	h<-read.gmt(paste0("/uhd/username/prova_nmi/genesets/",as.character(args[3]),".all.v6.1.entrez.gmt.txt"))}

  load(file = paste0("/uhd/username/prova_nmi/",args[2],"/consensi/",args[2],"_",as.character(args[1]),".RData"))
  D3<-D3l[[1]]
  comunit<-apply(D3,1,function(x)which(x!=0))
  comunit<-unique(comunit)
  comunitnomi<-lapply(comunit,function(x)nomigeni[x])
  
  load(file="/uhd/username/prova_nmi/Scripts/genilabel/dizionario.RData")
  name_id<-name_idl[[1]]
  

  rownames(name_id)<-name_id$SYMBOL
  comunitentrez<-lapply(comunitnomi,function(x)name_id[x,"ENTREZID"])
  controllona<-lapply(comunitentrez,function(x)length(x[is.na(x)]))
  which(controllona!=0)
    
  nomigeniid<-name_id[nomigeni,"ENTREZID"]
  
  
  xl<-lapply(comunitentrez,function(x)enricher(gene=x,universe=nomigeniid,TERM2GENE=h,pvalueCutoff = 0.01))
  save(xl,file =paste0("/uhd/username/prova_nmi/",args[2],"/",sub("^[^_]*_", "", args[2]),"_10020_",gs,"/",gs,"_comarr_mux_",sub("^[^_]*_", "", args[2]),"_10020_",as.character(args[1]),".RData"))
  
  xl<- xl[-which(sapply(xl, function(x)nrow(x)==0||is.null(x)))]
  save(xl,file =paste0("/uhd/username/prova_nmi/",args[2],"/",sub("^[^_]*_", "", args[2]),"_10020_",gs,"/",gs,"_damux_",sub("^[^_]*_", "", args[2]),"_10020_",as.character(args[1]),".RData"))