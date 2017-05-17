#' Function mediador_meta
#'
#' This function returns a data.frame with information about collective agreements between workers' unions
#' and companies or companies' unions
#' @param livre words to searched.
#' @param uf federative unit (state) 
#' @param cnpj company or organization tax number

#' @keywords mediador, labor agreements
#' @import RCurl
#' @import XML
#' @import stringi
#' @return A data.frame with metadata about the agreement.
#' @examples
#' mediador_meta(livre="industria qumicia",uf="SP") 
#' @export

mediador_meta<-function(livre="",uf="",cnpj=""){
  url1<-"http://www3.mte.gov.br/sistemas/mediador/ConsultarInstColetivo"
  url2 <- "http://www3.mte.gov.br/sistemas/mediador/ConsultarInstColetivo/getConsultaAvancada"
  
  ## Cria a handle, configura as opções do request e realiza o primeiro request, salvando os cookies para posteriores requests.
  curl = getCurlHandle()
  curlSetOpt(cookiejar="cookies.txt",   followlocation = TRUE, curl=curl)
  a=getURL(url1,curl=curl)
  
  ## Corpo do POST. Ele está limpo e será preenchido com os argumentos oferecidos.
  body <- list(
    nrCnpj="",
    nrCei="",
    noRazaoSocial="",
    dsCategoria="",
    tpRequerimento=c("acordo",
                     "acordoColetivoEspecificoPPE",
                     "acordoColetivoEspecificoDomingosFeriados",
                     "convencao",
                     "termoAditivoAcordo",
                     "termoAditivoConvecao",
                     "termoAditivoAcordoEspecificoPPE",
                     "termoAditivoAcordoEspecificoDomingoFeriado"),
    tpVigencia="2",
    sgUfDeRegistro="",
    dtInicioRegistro="",
    dtFimRegistro="",
    dtInicioVigenciaInstrumentoColetivo="",
    dtFimVigenciaInstrumentoColetivo="",
    tpAbrangencia="Todos os tipos",
    ufsAbrangidasTotalmente="",
    cdMunicipiosAbrangidos="",
    cdGrupo="",
    cdSubGrupo="",
    noTituloClausula="",
    utilizarSiracc="",
    pagina="1",
    qtdTotalRegistro="-1")
  
  ## Preenche o body com os argumentos.  
  body$nrCnpj<-cnpj
  body$dsCategoria<-livre
  body$sgUfDeRegistro<-uf
  body$ufsAbrangidasTotalmente<-uf
  
  ## Procedimento para identificar o número de páginas
  b=postForm(url2,.params = body, curl=curl)
  c<-htmlParse(b)
  val<-xpathSApply(c,"//*[@method='post']",xmlValue)
  val<-stri_extract_first_regex(val,"(?<=Resultado:\\s)\\d+")
  num<-as.numeric(val)
  max_pag<-ceiling(num/10)
  max_pag<-as.numeric(val)
  
  ## Cria um data.frame vazio para preenchê-lo com os dados.
  dt<-data.frame()
  
  ## Inicia o loop
  for(j in 1:max_pag){
    body$pagina<-j
    body$qtdTotalRegistro<-num
    b=postForm(url2,.params = body, curl=curl)
    c<-htmlParse(b)
    for (i in 2:11){
      registro<-xpathApply(c,paste0("//*[@id='grdInstrumentos']/table/thead/tr[",i,"]/td/div/table/tbody/tr[1]/td[2]/table//tr[1]/td[1]
                                    "),xmlValue,trim=T)[[1]]
      solicitacao<-xpathApply(c,paste0("//*[@id='grdInstrumentos']/table/thead/tr[",i,"]/td/div/table/tbody/tr[1]/td[2]/table//tr[1]/td[3]
                                       "),xmlValue,trim=T)[[1]]
      instrumento<-xpathApply(c,paste0("//*[@id='grdInstrumentos']/table/thead/tr[",i,"]/td/div/table/tbody/tr[2]/td[2]/table//tr[1]/td[1]
                                       "),xmlValue,trim=T)[[1]]
      vigencia<-xpathApply(c,paste0("//*[@id='grdInstrumentos']/table/thead/tr[",i,"]/td/div/table/tbody/tr[2]/td[2]/table//tr[1]/td[3]"),xmlValue,trim=T)[[1]]
      inicio.vigencia<-stri_extract_first_regex(vigencia,".*\\d",omit_no_match=FALSE)
      fim.vigencia<-stri_extract_first_regex(vigencia,"(?<=-\\s).*",omit_no_match=FALSE)
      partes<-xpathApply(c,paste0("//*[@id='grdInstrumentos']/table/thead/tr[",i,"]/td/div/table/tbody/tr[3]/td[2]"),xmlValue,trim=T)[[1]]
      parte1<-stri_trim(stri_split_regex(partes,"\r\n\\s*\r\n")[[1]][1])
      parte2<-stri_trim(stri_split_regex(partes,"\r\n\\s*\r\n")[[1]][2])
      dt1<-data.frame(registro,solicitacao,instrumento,inicio.vigencia,fim.vigencia,parte1,parte2)
      dt<-rbind(dt,dt1)
    }
  }
  return(dt)
}
