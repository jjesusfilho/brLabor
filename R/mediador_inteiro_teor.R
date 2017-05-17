#' Function  mediador_inteiro_teor
#'
#' This function downloads the whole texts of the agreements according to "solitacao" number
#' extracted through mediador_meta function
#' @param solicitacao number that works as an id of the document.
#' @param  download whether to download the document or not
#' @param vector wheter to create a vector of the documents as an R object.

#' @keywords mediador, labor agreements.
#' @import RCurl
#' @import XML
#' @import stringi
#' @return A file with the document and/or an R object with the text.
#' @examples
#' m<-mediador_inteiro_teor(solicitacao=c("MR087305_2016","MR035244_2009")) 
#' @export


mediador_inteiro_teor<-function(solicitacao,download=TRUE,vector=TRUE){
  
  url<-paste0("http://www3.mte.gov.br/sistemas/mediador/Resumo/ResumoVisualizar?NrSolicitacao=",solicitacao)
  
  ## Cria um objeto a parte para usá-lo a fim de nomear os arquivos a serem baixados
  s<-as.character(solicitacao)
  s<-stri_replace_first_regex(s,"/","_")
  
  ## Inicia o loop para baixar os textos.
  inteiro.teor<-""
  for(i in 1:length(s)){
    s1<-getURL(url[i])
    s1<-htmlParse(s1,encoding="UTF-8")
    s1<-xpathApply(s1,"//*[@class='titulo' or @align='justify' or @class='textosubgrupo'or @class='tituloClausula' or  @class='textonome' or @class='descricaoClausula']",xmlValue)
    s1<-toString(unlist(s1))
    s1<-stri_replace_all_regex(s1,"\\s+,","\n")
    if(vector==TRUE) inteiro.teor[i]<-s1
    if(download==TRUE) write(s1,paste0(s[i],".txt"))
  }
  return(inteiro.teor)
}