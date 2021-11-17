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

Cliente = 'FLORA'
#ABRINDO NAVEGADOR 
#eCaps <- list(chromeOptions = list(
#  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
#))
rD<-rsDriver(port = 4444L, browser = c('chrome'),chromever = '94.0.4606.61',verbose = TRUE, check = TRUE, #extraCapabilities=eCaps
) #Codigo que iremos passar o parametros para configurar o servidor
remDr<-rD[['client']] #Lista gerada, que é tipica de um cliente mexer no navegador
remDr$maxWindowSize()


lista<-c('https://www.compraflora.com.br/marca/albany/','https://www.compraflora.com.br/marca/assim/','https://www.compraflora.com.br/marca/brisa/',
         'https://www.compraflora.com.br/marca/francis/','https://www.compraflora.com.br/marca/karina/','https://www.compraflora.com.br/marca/kolene/',
         'https://www.compraflora.com.br/marca/mat-inset/','https://www.compraflora.com.br/marca/minuano/','https://www.compraflora.com.br/marca/neutrox/',
         'https://www.compraflora.com.br/marca/no-inset/','https://www.compraflora.com.br/marca/ox/','https://www.compraflora.com.br/marca/phytoderm/')

aux<-1
qtd<-length(lista)
for(c in aux:qtd){
remDr$navigate(lista[[aux]])
Sys.sleep(3)  
  
  for(n in 1:10){
  webElem<-remDr$findElements('tag name','body')[[1]]
  webElem$sendKeysToElement(list(key='end'))
  Sys.sleep(2)
  }
pd<-1  
tryCatch({
  for (c in 1:150){
      linkProduto<-remDr$findElements('class name','item-link')
      link<-linkProduto[[pd]]$getElementAttribute('href')              
      linkGeral<-c(link, linkGeral)
      print(linkGeral)
      pd<-pd+1
  }
},error=function(cond){
  print('Links Extraidos')
})
  
aux<-aux+1
}

pg<-1
qt<-length(linkGeral)-1
for(n in 1:qt){
  remDr$navigate(linkGeral[[pg]])
  
  nomeProduto<-remDr$findElements('class name', 'product-name')
  nomeProduto<-nomeProduto[[1]]$getElementText()
  print(nomeProduto)

  
  precoProduto<-remDr$findElements('id','price_display')
  precoProduto<-precoProduto[[1]]$getElementText()
  print(precoProduto)
  
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
  
  siteUrl<-linkGeral[[pg]]
  print(siteUrl)
  
  tryCatch({
    listaImagem<-remDr$findElements('class name','cloud-zoom')
    urlImagem<-listaImagem[[1]]$getElementAttribute('href')
    print(urlImagem)},
    error = function(cond){
      print(cond)
    })
  
  linhaProduto<-if(str_detect(nomeProduto, pattern = 'Phytoderm')==TRUE){
    'Phytoderm'
  }else if(str_detect(nomeProduto, pattern = 'OX') || str_detect(nomeProduto, pattern = 'Ox')==TRUE){
    'OX'
  }else if(str_detect(nomeProduto, pattern = 'No Inset')==TRUE){
    'No inset'
  }else if(str_detect(nomeProduto, pattern = 'Neutrox')==TRUE){
    'Neutrox'
  }else if(str_detect(nomeProduto, pattern = 'Minuano')==TRUE){
    'Minuano'
  }else if(str_detect(nomeProduto, pattern = 'Mat Inset')==TRUE){
    'Mat Inset'
  }else if(str_detect(nomeProduto, pattern = 'Kolene')==TRUE){
    'Kolene'
  }else if(str_detect(nomeProduto, pattern = 'Karina')==TRUE){
    'Karina'
  }else if(str_detect(nomeProduto, pattern = 'Francis')==TRUE){
    'Francis'
  }else if(str_detect(nomeProduto, pattern = 'Brisa')==TRUE){
    'Brisa'
  }else if(str_detect(nomeProduto, pattern = 'Assim')==TRUE){
    'Assim'
  }else if(str_detect(nomeProduto, pattern = 'Albany')==TRUE){
    'Albany'
  }else{
    '-'
  }
  print(linhaProduto)
  #CRIANDO FUNÇÃO 
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
  #PEGANDO 
  cont<-1
  tryCatch({
  for (m in 1:100){
    if(str_detect(remDr$findElements('xpath',paste0('//*[@id="single-product"]/div[2]/p[',cont,']'))[[1]]$getElementText(), pattern = 'Código')==TRUE){
      codProduto<-remDr$findElements('xpath',paste0('//*[@id="single-product"]/div[2]/p[',cont,']'))[[1]]$getElementText()
      codProduto<-substrRight(codProduto,7)
      print(codProduto)
      cont<-100
    }else{
      cont<-cont+1
      codProduto<-'-'
    }
  }
    },error=function(cond){
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
  

  pg<-pg+1
}




#FINALIZANDO MINERACAO, FECHANDO PAGINA WEB
remDr$close() #Fecho o navegador
rD$server$stop() #Fecha conexão realizada com servidor 
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE) #Forçando o R a fechar todas as portas Java (se necessário)
rm(rD, remDr) # excluindo os dados no final 
gc() #limpeza de disco
