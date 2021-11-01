library(RSelenium)
library(JavaGD)
library(httr)
library(stringr)

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

Cliente = 'MAHOGANY'
#ABRINDO NAVEGADOR 
rD<-rsDriver(port = 4444L, browser = c('chrome'),chromever = '94.0.4606.61',verbose = TRUE, check = TRUE) #Codigo que iremos passar o parametros para configurar o servidor
remDr<-rD[['client']] #Lista gerada, que é tipica de um cliente mexer no navegador
remDr$maxWindowSize()

remDr$navigate('https://www.mahogany.com.br/mahogany')#ABRINDO SITE PARA MINERAR 

for (c in 1:50){
webElem<-remDr$findElements('tag name','body')[[1]]
webElem$sendKeysToElement(list(key='end'))
Sys.sleep(2)
}

#PEGANDO LINKS DOS PRODUTOS 
linkProduto<-remDr$findElements('class name', 'prateleira__image-link')
aux<-1
qtd<-length(linkProduto)
for (i in aux:qtd+1){
  link<-linkProduto[[aux]]$getElementAttribute('href')
  linkGeral<-c(linkGeral,link)
  aux<-aux+1
  print(linkGeral)
}

#TRAZENDO TODOS OS DADOS 


aux<-3
tryCatch({
for (p in 1:qtd){
remDr$navigate(linkGeral[[aux]])

tryCatch({
nomeProduto<-remDr$findElements('class name', 'product__info--name')
nomeProduto<-nomeProduto[[2]]$getElementText()
print(nomeProduto)},
error = function(cond){
  print(cond)
})

linhaProduto<-remDr$findElements('class name','last')
linhaProduto<-linhaProduto[[1]]$getElementText()
print(linhaProduto)

volumeProduto<-nomeProduto
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
volumeProduto<-substrRight(volumeProduto,7)
print(volumeProduto)


precoProduto<-remDr$findElements('class name', 'skuBestPrice')
precoProduto<-precoProduto[[1]]$getElementText()
print(precoProduto)

codProduto<-remDr$findElements('class name','skuReference')
codProduto<-codProduto[[1]]$getElementText()
print(codProduto)

listaImagem <-remDr$findElements('tag name', 'img')
urlImagem<-listaImagem[[1]]$getElementAttribute('src')
urlImagem<-urlImagem[[1]]
print(urlImagem)


siteUrl<-linkGeral[[aux]]
print(siteUrl)
aux<-aux+1

dataMineracao<-Sys.Date()
tabelaDados<-rbind(tabelaDados, cbind(dataMineracao, Cliente, nomeProduto,linhaProduto, volumeProduto,precoProduto, codProduto, urlImagem, siteUrl))

Sys.sleep(4)
}
  },
  error = function(cond){
    print('Mineração finalizada')
  })

remDr$close() #Fecho o navegador
rD$server$stop() #Fecha conexão realizada com servidor 
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE) #Forçando o R a fechar todas as portas Java (se necessário)
rm(rD, remDr) # excluindo os dados no final 
gc() #limpeza de disco
