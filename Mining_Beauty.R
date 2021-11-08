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

Cliente = 'BEAUTYCOLOR'
#ABRINDO NAVEGADOR 
rD<-rsDriver(port = 4444L, browser = c('chrome'),chromever = '94.0.4606.61',verbose = TRUE, check = TRUE) #Codigo que iremos passar o parametros para configurar o servidor
remDr<-rD[['client']] #Lista gerada, que é tipica de um cliente mexer no navegador
remDr$maxWindowSize()
remDr$navigate('https://loja.beautycolorcompany.com.br/beautycolor-')#ABRINDO SITE PARA MINERAR 




linkProduto<-remDr$findElements('class','info-product')
aux<-1
qtd<-length(linkProduto)
for (c in aux:qtd+1){
  link<-linkProduto[[aux]]$getElementAttribute('href')
  linkGeral<-c(linkGeral, link)
  aux=aux+1
  print(linkGeral)
}

tryCatch({
for(x in 1:50){
webElem<-remDr$findElements('tag name','body')[[1]]
webElem$sendKeysToElement(list(key='end'))

#TROCAR DE PAGINA 
botaopagina<-remDr$findElements('class name','page-next')
botaopagina<-botaopagina[[1]]$clickElement()

linkProduto<-remDr$findElements('class','info-product')
aux<-1
qtd<-length(linkProduto)
for (c in aux:qtd+1){
  link<-linkProduto[[aux]]$getElementAttribute('href')
  linkGeral<-c(linkGeral, link)
  aux=aux+1
  print(linkGeral)
}
Sys.sleep(2)}
},error=function(cond){
  print('sem paginas a mais, iniciar mineração')
})

#MINERAÇÃO DE DADOS 
pg=3
x<-1
tryCatch({
for (p in 1:length(linkGeral)){
remDr$navigate(linkGeral[[pg]])
Sys.sleep(2)


nomeProduto<-remDr$findElements('class name','product-name')
nomeProduto<-nomeProduto[[5]]$getElementText()
print(nomeProduto)

precoProduto<-remDr$findElements('id','variacaoPreco')
precoProduto<-precoProduto[[1]]$getElementText()
print(precoProduto)


codProduto<-paste0('BEAUTY0',x)
x<-x+1
print(codProduto)

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

linhaProduto<-remDr$findElements('class name','breadcrumb-item')
linhaProduto<-linhaProduto[[3]]$getElementText()
print(linhaProduto)

urlImagem<-remDr$findElements('class name','zoomImg')
urlImagem<-urlImagem[[1]]$getElementAttribute('src')
print(urlImagem)


siteUrl<-linkGeral[[pg]]
print(siteUrl)

pg<-pg+1

#COLOCANDO DADOS EM UMA TABELA.
dataMineracao<-Sys.Date()

tabelaDados<-rbind(tabelaDados, cbind(dataMineracao, Cliente, nomeProduto,linhaProduto, volumeProduto,precoProduto, codProduto, urlImagem, siteUrl))
}
},error = function(cond){
  print("Mineração finalizada")
})

#FINALIZANDO MINERACAO, FECHANDO PAGINA WEB
remDr$close() #Fecho o navegador
rD$server$stop() #Fecha conexão realizada com servidor 
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE) #Forçando o R a fechar todas as portas Java (se necessário)
rm(rD, remDr) # excluindo os dados no final 
gc() #limpeza de disco

                          