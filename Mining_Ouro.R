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

Cliente = 'OUROFINO'
#ABRINDO NAVEGADOR 
#eCaps <- list(chromeOptions = list(
#  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
#))
rD<-rsDriver(port = 4444L, browser = c('chrome'),chromever = '94.0.4606.61',verbose = TRUE, check = TRUE, #extraCapabilities=eCaps
) #Codigo que iremos passar o parametros para configurar o servidor
remDr<-rD[['client']] #Lista gerada, que é tipica de um cliente mexer no navegador
remDr$maxWindowSize()

remDr$navigate('https://www.ourofinosaudeanimal.com/produtos/')
Sys.sleep(3)

for(n in 1:15){
  webElem<-remDr$findElements('tag name','body')[[1]]
  webElem$sendKeysToElement(list(key='end'))
  
  for (i in 1:8){
    webElem$sendKeysToElement(list(key='up_arrow'))
  }
  
  botaopagina<-remDr$findElements('class name','btn-load-more')
  botaopagina<-botaopagina[[1]]$clickElement()
  Sys.sleep(4)
}


#Extraindo links 
linkProduto<-remDr$findElements('class name', 'link')
aux<-1
qtd<-length(linkProduto)
for (i in aux:qtd){
  link<-linkProduto[[aux]]$getElementAttribute('href')
  linkGeral<-c(linkGeral,link)
  aux<-aux+1
  print(linkGeral)
}


#coletando dados
pg<-25
qtd<-length(linkGeral)
for(d in pg:qtd){
  remDr$navigate(linkGeral[[pg]])
  
  print(nomeProduto<-remDr$findElements('class name','titulo')[[1]]$getElementText())
  print(linhaProduto<-remDr$findElements('class name','label__categoria')[[1]]$getElementText())
  print(precoProduto<-0)
  print(volumeProduto<-0)
  print(codProduto<-paste0('OURO',pg))
  print(siteUrl<-linkGeral[[pg]])
  tryCatch({
    print(urlImagem<-remDr$findElements('class name','imagem')[[1]]$getElementAttribute('src'))},
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
  
  pg<-pg+1
}

#FINALIZANDO MINERACAO, FECHANDO PAGINA WEB
remDr$close() #Fecho o navegador
rD$server$stop() #Fecha conexão realizada com servidor 
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE) #Forçando o R a fechar todas as portas Java (se necessário)
rm(rD, remDr) # excluindo os dados no final 
gc() #limpeza de disco
