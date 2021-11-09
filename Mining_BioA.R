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

Cliente = 'BIOAGE'
#ABRINDO NAVEGADOR 
rD<-rsDriver(port = 4444L, browser = c('chrome'),chromever = '94.0.4606.61',verbose = TRUE, check = TRUE) #Codigo que iremos passar o parametros para configurar o servidor
remDr<-rD[['client']] #Lista gerada, que é tipica de um cliente mexer no navegador
remDr$maxWindowSize()


lista<-c('https://www.bioage.com.br/tratamentos/corporal','https://www.bioage.com.br/tratamentos/facial','https://www.bioage.com.br/tratamentos/fotoprotecao','https://www.bioage.com.br/tratamentos/kits')
linha<-c('CORPORAL','FACIAL','FOTO PROTECAO','KITS')

tt<-1
for (i in tt:5){
remDr$navigate(lista[[tt]])

aux<-1
tryCatch({
  for (c in 1:50){
    linkProduto<-remDr$findElements('xpath',paste0('//*[@id="category-container"]/div[2]/div[2]/div/div[',aux,']/div/div[1]/div/a'))
    link<-linkProduto[[1]]$getElementAttribute('href')              
    linkGeral<-c(link, linkGeral)
    print(linkGeral)
    aux<-aux+1
  }
},error = function(cond){
  print('Links extraidos')
})

Sys.sleep(2)

#Movimentando e mudando de pagina
webElem<-remDr$findElements('tag name','body')[[1]]
webElem$sendKeysToElement(list(key='end'))

for (p in 1:8){
  webElem$sendKeysToElement(list(key='up_arrow'))
}

#MUDAR DE PAGINA
if (length(botaopagina<-remDr$findElements('class name','i-next'))>0){
  
  tryCatch({
  for (p in 1:30){
  botaopagina<-remDr$findElements('class name','i-next')
  botaopagina<-botaopagina[[2]]$clickElement()
  
  aux<-1
  tryCatch({
    for (c in 1:50){
      linkProduto<-remDr$findElements('xpath',paste0('//*[@id="category-container"]/div[2]/div[2]/div/div[',aux,']/div/div[1]/div/a'))
      link<-linkProduto[[1]]$getElementAttribute('href')              
      linkGeral<-c(link, linkGeral)
      print(linkGeral)
      aux<-aux+1
    }
  },error = function(cond){
    print('Links extraidos')
  })
}
    },error = function(cond){
    print('Proxima pagina')
  })
  Sys.sleep(2)
}else{
  print('Não há paginas nesta linha')
}
tt<-tt+1
}
Sys.sleep(2)


#INICIANDO MINERAÇÃO DE DADOS
tt<-1
qtd<-length(linkGeral)
for(x in tt:qtd-1){
  remDr$navigate(linkGeral[[tt]])
  
  nomeProduto<-remDr$findElements('xpath','/html/body/div[1]/div/div[8]/div[2]/div[1]/div[1]/div[2]/h1')
  nomeProduto<-nomeProduto[[1]]$getElementText()
  print(nomeProduto)
  
  if(length(precoProduto<-remDr$findElements('xpath','/html/body/div[1]/div/div[8]/div[2]/div[1]/div[1]/div[2]/div[2]/a/div/p'))>0){
  precoProduto<-remDr$findElements('xpath','/html/body/div[1]/div/div[8]/div[2]/div[1]/div[1]/div[2]/div[2]/a/div/p')
  precoProduto<-precoProduto[[1]]$getElementText()
  print(precoProduto)
  }else{
    precoProduto<-remDr$findElements('class name','text-primary')
    precoProduto<-precoProduto[[7]]$getElementText()
    print(precoProduto)
  }
  
  linhaProduto<-remDr$findElements('xpath', '/html/body/div[1]/div/div[8]/div[1]/div/span[3]/a')
  linhaProduto<-linhaProduto[[1]]$getElementText()                                
  print(linhaProduto)
  
  codProduto<-remDr$findElements('class name','text-muted')
  codProduto<-codProduto[[1]]$getElementText()
  
  tryCatch({
    substrRight <- function(x, n){
      substr(x, nchar(x)-n+1, nchar(x))
    }
    codProduto<-substrRight(codProduto,8)
    print(codProduto)},
    error = function(cond){
      print(cond)
    })
  
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
  
  tryCatch({
    listaImagem<-remDr$findElements('xpath','/html/body/div[1]/div/div[8]/div[2]/div[1]/div[1]/div[1]/section/div/div/div/div/img[1]')
    urlImagem<-listaImagem[[1]]$getElementAttribute('src')
    print(urlImagem)},
    error = function(cond){
      print(cond)
    })
  
  siteUrl<-linkGeral[[tt]]
  print(siteUrl)
  
  
  dataMineracao<-Sys.Date()
  tabelaDados<-rbind(tabelaDados, cbind(dataMineracao, Cliente, nomeProduto,linhaProduto, volumeProduto,precoProduto, codProduto, urlImagem, siteUrl))
  
  tt<-tt+1
}


#FINALIZANDO MINERACAO, FECHANDO PAGINA WEB
remDr$close() #Fecho o navegador
rD$server$stop() #Fecha conexão realizada com servidor 
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE) #Forçando o R a fechar todas as portas Java (se necessário)
rm(rD, remDr) # excluindo os dados no final 
gc() #limpeza de disco