#' Function acidente_cnae
#'
#' This function returns 
#' @param cnae vector of Brazilian Code of Economic Activity corresponding to the ISIC

#' @keywords labor accidents
#' @import jsonlite
#' @import stringr
#' @return A data.frame with 7 variables with the information available.
#' @examples
#' acidente_cnae(cnae=c(20,21,22)) # Extract information from chemical, pharmaceutical and rubber industries.

#' @export

acidente_cnae<-function(cnae){
url<-"http://dadosabertos.dataprev.gov.br/opendata/act10/formato=json"
a<-readLines(file(url, encoding="ISO-8859-1"), warn=FALSE)
b<-fromJSON(a)
c<-b$nodes$node
cnae<-paste0(cnae,collapse = "|")
cnae<-paste0("^(",cnae,")")
c<-c[str_which(c$CNAE,cnae),]
return(c)
}

