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
################################### EXTRAINDO DADOS ###################################
##################################    DEPIL BELA    ###################################
tt<-1
lista<-c(sprintf('https://loja.grupobioclean.com.br/loja/catalogo.php?loja=483674&categoria=1&pg=%s',tt),sprintf('https://loja.grupobioclean.com.br/loja/catalogo.php?loja=483674&categoria=14&pg=%s',tt))
remDr$navigate(lista[[tt]])

tryCatch({
fim<-remDr$findElements('xpath','/html/body/main/div/section/div/div[1]/div[2]/span[6]/a')
fim<-fim[[1]]$getElementAttribute('href')
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

fim<-substrRight(fim,1)
fim},error = function(cond){
  print('sem paginas')
  fim<-2
})

tt<-1
for (i in tt:fim){
  remDr$navigate(sprintf('https://loja.grupobioclean.com.br/loja/catalogo.php?loja=483674&categoria=1&pg=%s',tt))
  
  Sys.sleep(4)
  #EXTRAINDO LINKS
  aux<-1
  tryCatch({
    if(length(remDr$findElements('xpath',paste0('/html/body/main/div/section/div/div[3]/div/div/div[',aux,']/div/figure/a')))>0){
    for (c in 1:50){
      linkProduto<-remDr$findElements('xpath',paste0('/html/body/main/div/section/div/div[3]/div/div/div[',aux,']/div/figure/a'))
      link<-linkProduto[[1]]$getElementAttribute('href')              
      linkGeral<-c(link, linkGeral)
      print(linkGeral)
      aux<-aux+1
      }
      }else {
        print('não há paginas')
      }
    },error = function(cond){
    print('Links extraidos')
  })
  tt<-tt+1
}

################################### EXTRAINDO DADOS ###################################
##################################    RAAVI         ###################################

tt<-1
lista<-c(sprintf('https://loja.grupobioclean.com.br/loja/catalogo.php?loja=483674&categoria=14&pg=%s',tt))
remDr$navigate(lista[[tt]])

fim<-remDr$findElements('xpath','/html/body/main/div/section/div/div[1]/div[2]/span[6]/a')
fim<-fim[[1]]$getElementAttribute('href')
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

fim<-substrRight(fim,1)
fim


tt<-1
for (i in tt:fim){
  remDr$navigate(sprintf('https://loja.grupobioclean.com.br/loja/catalogo.php?loja=483674&categoria=14&pg=%s',tt))
  
  Sys.sleep(4)
  #EXTRAINDO LINKS
  aux<-1
  tryCatch({
    if(length(remDr$findElements('xpath',paste0('/html/body/main/div/section/div/div[3]/div/div/div[',aux,']/div/figure/a')))>0){
      for (c in 1:50){
        linkProduto<-remDr$findElements('xpath',paste0('/html/body/main/div/section/div/div[3]/div/div/div[',aux,']/div/figure/a'))
        link<-linkProduto[[1]]$getElementAttribute('href')              
        linkGeral<-c(link, linkGeral)
        print(linkGeral)
        aux<-aux+1
      }
    }else {
      print('não há paginas')
    }
  },error = function(cond){
    print('Links extraidos')
  })
  tt<-tt+1
}

################################### EXTRAINDO DADOS ###################################
##################################    DEPIL HOMME  ###################################

tt<-1
lista<-c(sprintf('https://loja.grupobioclean.com.br/depil-homme?loja=483674&categoria=29&pg=%s',tt))
remDr$navigate(lista[[tt]])


fim<-remDr$findElements('xpath','/html/body/main/div/section/div/div[1]/div[2]/span[6]/a')
fim<-fim[[1]]$getElementAttribute('href')
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

fim<-substrRight(fim,1)
fim


tt<-1
for (i in tt:fim){
  remDr$navigate(sprintf('https://loja.grupobioclean.com.br/depil-homme?loja=483674&categoria=29&pg=%s',tt))
  
  Sys.sleep(4)
  #EXTRAINDO LINKS
  aux<-1
  tryCatch({
    if(length(remDr$findElements('xpath',paste0('/html/body/main/div/section/div/div[3]/div/div/div[',aux,']/div/figure/a')))>0){
      for (c in 1:50){
        linkProduto<-remDr$findElements('xpath',paste0('/html/body/main/div/section/div/div[3]/div/div/div[',aux,']/div/figure/a'))
        link<-linkProduto[[1]]$getElementAttribute('href')              
        linkGeral<-c(link, linkGeral)
        print(linkGeral)
        aux<-aux+1
      }
    }else {
      print('não há paginas')
    }
  },error = function(cond){
    print('Links extraidos')
  })
  tt<-tt+1
}












#FINALIZANDO MINERACAO, FECHANDO PAGINA WEB
remDr$close() #Fecho o navegador
rD$server$stop() #Fecha conexão realizada com servidor 
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE) #Forçando o R a fechar todas as portas Java (se necessário)
rm(rD, remDr) # excluindo os dados no final 
gc() #limpeza de disco
