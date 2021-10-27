#' rais
#'
#' This function returns microdata of Brazilian workers
#' @param uf vector of Brazilian federation unites (states + federal district)
#' @param CNAE Brazilian Code of Economic Activity corresponding to ISIC
#' @param CBO Brazilian Standard Classification of Ocupations
#' @param year year of data to download.
#' @keywords rais, ocupations, employment, jobs, workers
#' @return A data frame with rais data.
#' @examples
#' df<-rais(uf=c("AC","SC"),CNAE="22",CBO="21",year=2014)

#' @export

rais<-function(uf=NULL,CNAE="",CBO="",year=NULL){

  stopifnot(length(year)==1,is.numeric(year))

  u<-paste0("ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/",year,"/")

  list_files <- curl::new_handle()

  curl::handle_setopt(list_files, ftp_use_epsv = TRUE, dirlistonly = TRUE)

  con <- curl::curl(url = u, "r", handle = list_files)

  tbl <- read.table(con, stringsAsFactors=FALSE, fill=TRUE)

  close(con)

  tbl<- tbl %>%
       dplyr::filter(stringr::str_detect(V1,paste0(uf,collapse = "|"))) %>%
       dplyr::pull("V1") %>%
       paste0(u,.)


  urls <- paste0(u, tbl)

  urls<-stringr::str_subset(urls,"[^(Estb|ESTB)]")

  fls <- basename(urls)

  fil<-stringr::str_replace(fls,"\\..*",".txt")



  CNAE<-paste0(CNAE,collapse = "|")

  CBO<-paste0(CBO,collapse = "|")

  f<-function(x,pos){

    y <- stringr::str_which(stringr::str_trim(x$X9),paste0("^",CNAE))

    z <- stringr::str_which(stringr::str_trim(x$X8),paste0("^",CBO))

    x[intersect(y,z),]
  }

  c <- data.frame()



  for (i in 1:length(fls)){

    file.remove(fls[i])

    file.remove(fil[i])

    download.file(urls[i],destfile = fls[i])

    archive::archive_extract(fls[i],fil[i])

    a <- list.files(fil[i],full.names=TRUE)

    df<-readr::read_delim_chunked(a,
                                  col_names=FALSE,
                                  readr::DataFrameCallback$new(f),
                                  chunk_size = 1000,delim=";")

    if(year<2015){
      names(df)<-paste0("X",1:45)
    }
    else{
      names(df)<-paste0("X",1:57)
    }

    if(nrow(df)>0) df$uf<-stringr::str_extract(fls[i],"\\D+")

    df <- as.data.frame(df,stringsAsFactor=F)

    c<-rbind(c,df)


    file.remove(fls[i])

    file.remove(fil[i])
  }

  return(c)
}
