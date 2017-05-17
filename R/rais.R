#' Function rais
#'
#' This function returns microdata of Brazilian workers
#' @param uf vector of Brazilian federation unites (states + federal district)
#' @param CNAE Brazilian Code of Economic Activity corresponding to ISIC
#' @param CBO Brazilian Standard Classification of Ocupations
#' @param year year of data to download.

#' @keywords rais, ocupations, employment, jobs, workers
#' @import curl
#' @import stringr
#' @import readr
#' @return A data.frame with 
#' @examples
#' rais(uf=c("AC","SC"),CNAE="22",CBO="21",year=2014)

#' @export

rais<-function(uf=NULL,CNAE="",CBO="",year=NULL){
  stopifnot(length(year)==1,is.numeric(year))
  u<-paste0("ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/",year,"/")
  h <- curl::new_handle(dirlistonly=TRUE)
  con <- curl::curl(u, "r", h)
  tbl <- read.table(con, stringsAsFactors=FALSE, fill=TRUE)
  close(con)
  tbl<-tbl[str_which(tbl$V1,paste0(uf,collapse="|")),1]
  urls <- paste0(u, tbl)
  urls<-str_subset(urls,"[^(Estb|ESTB)]")
  fls <- basename(urls)
  fil<-str_replace(fls,"\\..*",".txt")
  
  CNAE<-paste0(CNAE,collapse = "|")
  CBO<-paste0(CBO,collapse = "|")
  
  f<-function(x,pos){
    y<-str_which(str_trim(x$X9),paste0("^",CNAE))
    z<-str_which(str_trim(x$X8),paste0("^",CBO))
    x[intersect(y,z),]
  }
  c<-data.frame()
  
  for (i in 1:length(fls)){
    file.remove(fls[i])
    file.remove(fil[i])
    download.file(urls[i],destfile = fls[i])
    system(paste0("7z X ",fls[i]))
    a<-read_delim_chunked(fil[i], col_names=FALSE, DataFrameCallback$new(f),
                          chunk_size = 1000,delim=";")
    
    if(year<2015){
      names(a)<-paste0("X",1:45)
    }
    else{
      names(a)<-paste0("X",1:57)
    }
    
    if(nrow(a)>0) a$uf<-str_extract(fls[i],"\\D+")
    
    a<-as.data.frame(a,stringsAsFactor=F)
    c<-rbind(c,a)
    file.remove(fls[i])
    file.remove(fil[i])
  }
  
  return(c)
}