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
#eCaps <- list(chromeOptions = list(
#  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
#))
rD<-rsDriver(port = 4444L, browser = c('chrome'),chromever = '94.0.4606.61',verbose = TRUE, check = TRUE, #extraCapabilities=eCaps
             ) #Codigo que iremos passar o parametros para configurar o servidor
remDr<-rD[['client']] #Lista gerada, que é tipica de um cliente mexer no navegador
remDr$maxWindowSize()

remDr$navigate('https://centralsulquimica.com.br/produto/')


#Movimentando e mudando de pagina
webElem<-remDr$findElements('tag name','body')[[1]]
webElem$sendKeysToElement(list(key='end'))

for (p in 1:8){
  webElem$sendKeysToElement(list(key='up_arrow'))
}


botaopagina<-remDr$findElements('class name','page-numbers')
botaopagina<-botaopagina[[2]]$clickElement()



#FINALIZANDO MINERACAO, FECHANDO PAGINA WEB
remDr$close() #Fecho o navegador
rD$server$stop() #Fecha conexão realizada com servidor 
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE) #Forçando o R a fechar todas as portas Java (se necessário)
rm(rD, remDr) # excluindo os dados no final 
gc() #limpeza de disco
