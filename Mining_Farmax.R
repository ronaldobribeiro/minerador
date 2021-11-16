library(RSelenium)
library(JavaGD)
library(httr)
library(stringr)
library(RODBC)
library(odbc)
library(DBI)


#binman::list_versions('chromedriver')

#CRIANDO TABELAS QUE SERÃO UTILIZADAS 
linkGeral <- data.frame(link=as.character(character()),
                        stringsAsFactors=FALSE) 

tabelaDados<-data.frame(dataMineracao = as.Date(character()),
                        Cliente = as.character(character()),
                        nomeProduto = as.character(character()),
                        linhaProduto = as.character(character()),
                        volumeProduto = as.character(character()),
                        precoProduto = as.character(character()),
                        codProduto = as.character(character()),
                        urlImagem = as.character(character()),
                        siteUrl = as.character(character()))

Cliente = 'FARMAX'
#ABRINDO NAVEGADOR 
#eCaps <- list(chromeOptions = list(
#  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
#))
rD<-rsDriver(port = 4444L, browser = c('chrome'),chromever = '94.0.4606.61',verbose = TRUE, check = TRUE, #extraCapabilities=eCaps
) #Codigo que iremos passar o parametros para configurar o servidor
remDr<-rD[['client']] #Lista gerada, que é tipica de um cliente mexer no navegador
remDr$maxWindowSize()


remDr$navigate('https://farmax.com.br/products/')

webElem<-remDr$findElements('tag name','body')[[1]]
webElem$sendKeysToElement(list(key='end'))


aux<-1
tryCatch({
  for (c in 1:300){
    linkProduto<-remDr$findElements('xpath',paste0('/html/body/main/section[5]/div/article[',aux,']/a[1]'))
    link<-linkProduto[[1]]$getElementAttribute('href')              
    linkGeral<-c(link, linkGeral)
    print(linkGeral)
    aux<-aux+1
  }
},error = function(cond){
  print('Links extraidos')
})

p<-1
qtd<-length(linkGeral)-1
for(c in p:qtd){
  remDr$navigate(linkGeral[[p]])
  Sys.sleep(3)
  nomeProduto<-remDr$findElements('class name','e-mainProduct__title')
  nomeProduto<-nomeProduto[[1]]$getElementText()
  print(nomeProduto)
  
  linhaProduto<-0
  
  precoProduto<-0
  
  siteUrl<-linkGeral[[p]]
  print(siteUrl)
  
  tryCatch({
    volumeProduto<-nomeProduto
    substrRight <- function(x, n){
      substr(x, nchar(x)-n+1, nchar(x))
    }
    volumeProduto<-substrRight(volumeProduto,6)
    print(volumeProduto)},
    error = function(cond){
      print(cond)
    })
  
  codProduto<-paste0('FARMAX',p)
  
  tryCatch({
    listaImagem<-remDr$findElements('class name','wp-post-image')
    urlImagem<-listaImagem[[1]]$getElementAttribute('src')
    print(urlImagem)},
    error = function(cond){
      print(cond)
    })
  
  #CRIANDO TABELAS DE DADOS
  dataMineracao<-Sys.Date()
  
  #REMOVER ACENTOS 
  nomeProduto<-chartr('áéíóÁÉÍÓÂÊÎÔâêîôãõÃÕçÇÀà','aeioAEIOAEIOaeioaoAOcCAa',nomeProduto)
  linhaProduto<-chartr('áéíóÁÉÍÓÂÊÎÔâêîôãõÃÕçÇÀà','aeioAEIOAEIOaeioaoAOcCAa',linhaProduto)
  #REMOVER APOSTROFO
  nomeProduto<-str_replace_all(nomeProduto,"[']"," ")
  linhaProduto<-str_replace_all(linhaProduto,"[']"," ")
  
  tabelaDados<-rbind(tabelaDados, cbind(dataMineracao, Cliente, nomeProduto,linhaProduto, volumeProduto,precoProduto, codProduto, urlImagem, siteUrl))
  
  p<-p+1
}

#FINALIZANDO MINERACAO, FECHANDO PAGINA WEB
remDr$close() #Fecho o navegador
rD$server$stop() #Fecha conexão realizada com servidor 
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE) #Forçando o R a fechar todas as portas Java (se necessário)
rm(rD, remDr) # excluindo os dados no final 
gc() #limpeza de disco
