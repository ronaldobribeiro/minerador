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

Cliente = 'DEPIL-BELA'
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

tryCatch({
fim<-remDr$findElements('xpath','/html/body/main/div/section/div/div[1]/div[2]/span[6]/a')
fim<-fim[[1]]$getElementAttribute('href')},error=function(cond){
  print('sem paginas')
})
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

fim<-substrRight(fim,1)

if(length(fim)==0){
  fim=1
}else{
  fim = fim
}


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


################################### MINERANDO OS DADOS ###################################
##########################################################################################

p<-1
qtd<-length(linkGeral)
qtd<-qtd-1

for(x in p:qtd){
  remDr$navigate(linkGeral[[p]])
  Sys.sleep(2)
  
  
  nomeProduto<-remDr$findElements('class name','hidden-sm')
  nomeProduto<-nomeProduto[[1]]$getElementText()
  print(nomeProduto)

  precoProduto<-remDr$findElements('id','variacaoPreco')  
  precoProduto<-precoProduto[[1]]$getElementText()
  print(precoProduto)
  
  linhaProduto<-remDr$findElements('class name','dados-valor')
  linhaProduto<-linhaProduto[[1]]$getElementText()
  print(linhaProduto)
  
  codProduto<-remDr$findElements('class name','dados-valor')
  codProduto<-codProduto[[2]]$getElementText()
  print(codProduto)
  
  tryCatch({
    volumeProduto<-nomeProduto
    substrRight <- function(x, n){
      substr(x, nchar(x)-n+1, nchar(x))
    }
    volumeProduto<-substrRight(volumeProduto,16)
    print(volumeProduto)},
    error = function(cond){
      print(cond)
    })
  
  tryCatch({
    listaImagem<-remDr$findElements('class name','cloud-zoom')
    urlImagem<-listaImagem[[1]]$getElementAttribute('href')
    print(urlImagem)},
    error = function(cond){
      print(cond)
    })
  
  siteUrl<-linkGeral[[p]]
  print(siteUrl)

  p<-p+1
  
  #CRIANDO TABELAS DE DADOS
  dataMineracao<-Sys.Date()
  
  #REMOVER ACENTOS 
  nomeProduto<-chartr('áéíóÁÉÍÓÂÊÎÔâêîôãõÃÕçÇÀà','aeioAEIOAEIOaeioaoAOcCAa',nomeProduto)
  linhaProduto<-chartr('áéíóÁÉÍÓÂÊÎÔâêîôãõÃÕçÇÀà','aeioAEIOAEIOaeioaoAOcCAa',linhaProduto)
  #REMOVER APOSTROFO
  nomeProduto<-str_replace_all(nomeProduto,"[']"," ")
  linhaProduto<-str_replace_all(linhaProduto,"[']"," ")
  
  tabelaDados<-rbind(tabelaDados, cbind(dataMineracao, Cliente, nomeProduto,linhaProduto, volumeProduto,precoProduto, codProduto, urlImagem, siteUrl))
  
  }



#FINALIZANDO MINERACAO, FECHANDO PAGINA WEB
remDr$close() #Fecho o navegador
rD$server$stop() #Fecha conexão realizada com servidor 
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE) #Forçando o R a fechar todas as portas Java (se necessário)
rm(rD, remDr) # excluindo os dados no final 
gc() #limpeza de disco
