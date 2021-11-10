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

Cliente = 'CENTRAL SUL'
#ABRINDO NAVEGADOR 
#eCaps <- list(chromeOptions = list(
#  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
#))
rD<-rsDriver(port = 4444L, browser = c('chrome'),chromever = '94.0.4606.61',verbose = TRUE, check = TRUE, #extraCapabilities=eCaps
             ) #Codigo que iremos passar o parametros para configurar o servidor
remDr<-rD[['client']] #Lista gerada, que é tipica de um cliente mexer no navegador
remDr$maxWindowSize()

remDr$navigate('https://centralsulquimica.com.br/produto/')

#Extraindo caminhos.
tryCatch({
for (p in 1:50){
aux<-1
qtd<-length(linkProduto)
tryCatch({
  for (c in 1:qtd){
    linkProduto<-remDr$findElements('class name','item')
    link<-linkProduto[[aux]]$getElementAttribute('href')              
    linkGeral<-c(link, linkGeral)
    print(link)
    aux<-aux+1
  }
},error = function(cond){
  print('Links extraidos')
})


#Movimentando e mudando de pagina
webElem<-remDr$findElements('tag name','body')[[1]]
webElem$sendKeysToElement(list(key='end'))

for (p in 1:8){
  webElem$sendKeysToElement(list(key='up_arrow'))
}


botaopagina<-remDr$findElements('class name','next')
botaopagina<-botaopagina[[1]]$clickElement()

Sys.sleep(4)

}
},error=function(cond){
  print('Links Extraidos, iniciar mineração' )
})



#INICIAR EXTRACAO

pg<-1
qtd<-length(linkGeral)-1

for (c in 1:qtd){
  remDr$navigate(linkGeral[[pg]])
  
  Sys.sleep(3)
  nomeProduto<-remDr$findElements('xpath','//*[@id="produtos"]/div/div/div[1]/div[1]/div[1]/h1')
  nomeProduto<-nomeProduto[[1]]$getElementText()
  print(nomeProduto)
  
  precoProduto<-0
  
  if(length(volumeProduto<-remDr$findElements('class name','volume'))>0){
    volumeProduto<-remDr$findElements('class name','volume')
    volumeProduto<-volumeProduto[[1]]$getElementText()
    print(volumeProduto)
  }else{
    volumeProduto<-0
  }
  
  
  if(length(remDr$findElements('class name','referencia'))>0){
  codProduto<-remDr$findElements('class name','referencia')
  codProduto<-codProduto[[1]]$getElementText()
  substrLeft <- function(x, n){
    substr(x, n, nchar(x))
  }
  
  codProduto<-substrLeft(codProduto,6)
  print(codProduto)
  } else {
    codProduto<-0
  }
  
  linhaProduto<-remDr$findElements('class name','ativo')
  linhaProduto<-linhaProduto[[2]]$getElementText()
  print(linhaProduto)

  
  listaImagem<-remDr$findElements('xpath','//*[@id="produtos"]/div/div/div[1]/div[1]/div[2]/div[1]/div/div/div/img')
  urlImagem<-listaImagem[[1]]$getElementAttribute('src')
  print(urlImagem)
  
  
  siteUrl<-linkGeral[[pg]]
  print(siteUrl)
  
  dataMineracao<-Sys.Date()
  tabelaDados<-rbind(tabelaDados, cbind(dataMineracao, Cliente, nomeProduto,linhaProduto, volumeProduto,precoProduto, codProduto, urlImagem, siteUrl))
  
  
  
  pg<-pg+1
}

#FINALIZANDO MINERACAO, FECHANDO PAGINA WEB
remDr$close() #Fecho o navegador
rD$server$stop() #Fecha conexão realizada com servidor 
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE) #Forçando o R a fechar todas as portas Java (se necessário)
rm(rD, remDr) # excluindo os dados no final 
gc() #limpeza de disco
