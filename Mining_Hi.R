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

Cliente = 'HINODE'
#ABRINDO NAVEGADOR 
rD<-rsDriver(port = 4444L, browser = c('chrome'),chromever = '94.0.4606.61',verbose = TRUE, check = TRUE) #Codigo que iremos passar o parametros para configurar o servidor
remDr<-rD[['client']] #Lista gerada, que é tipica de um cliente mexer no navegador
remDr$maxWindowSize()


minerador=1



for (n in 1:6){
#paginas para minerar
pagina<-c('https://www.hinode.com.br/cabelo','https://www.hinode.com.br/corpo-e-banho','https://www.hinode.com.br/fragrancias', 
          'https://www.hinode.com.br/maquiagem','https://www.hinode.com.br/nutricao-e-performance')
#CATEGORIAS DE MINERACAO
linha<-c('CABELOS', 'CORPO & BANHO','FRAGRANCIAS', 'MAQUIAGENS', 'NUTRICAO E PERFPORMANCE')


remDr$navigate(pagina[[minerador]])#ABRINDO SITE PARA MINERAR 





#ABRINDO TODAS AS PÁGINAS
tryCatch({
for(i in 1:10){
webElem<-remDr$findElements('tag name','body')[[1]]
webElem$sendKeysToElement(list(key='end'))

for (p in 1:60){
webElem$sendKeysToElement(list(key='up_arrow'))
}
Sys.sleep(3)
botaopagina<-remDr$findElements('css selector','body > div.render-container.render-route-store-search-department > div > div.vtex-store__template.bg-base > div > div:nth-child(5) > div > div > section > div.relative.justify-center.flex > div > div.vtex-flex-layout-0-x-flexRow.vtex-flex-layout-0-x-flexRow--container-products > div > div.pr0.items-stretch.flex-grow-1.flex > div > div:nth-child(3) > div > div > div > div > div > a')
botaopagina<-botaopagina[[1]]$clickElement()
Sys.sleep(3)
}
},error =function(cond){
  print(cond)
})

#PEGADO TODOS OS LINKS DOS PRODUTOS
linkProduto<-remDr$findElements('class','vtex-product-summary-2-x-clearLink')
aux<-1
qtd<-length(linkProduto)
for (c in aux:qtd+1){
link<-linkProduto[[aux]]$getElementAttribute('href')
linkGeral<-c(linkGeral, link)
aux=aux+1
print(linkGeral)
}




pg<-3
qtd<-length(linkGeral)


tryCatch({
for (x in 1:qtd+1){
  remDr$navigate(linkGeral[[pg]])
  
  if(
    length(tryCatch({
      remDr$findElements('class name','vtex-rich-text-0-x-strong')},
      error=function(cond){
        print(cond)
      }))>0){
    
    tryCatch({
      dataMineracao<-Sys.Date()
      nomeProduto<-"PRODUTO INDISPONIVEL"
      print(nomeProduto)
      volumeProduto<-"0"
      print(volumeProduto)
      precoProduto<-"PRODUTO INDISPONIVEL"
      print(precoProduto)
      codProduto<-"PRODUTO INDISPONIVEL"
      print(codProduto)
      linhaProduto<-ifelse(minerador==1,linha[[1]],
                           ifelse(minerador==2, linha[[2]],
                                  ifelse(minerador==3, linha[[3]],
                                         ifelse(minerador==4, linha[[4]],linha[[5]]))))
      print(linhaProduto)
      urlImagem<-'PRODUTO INDISPONIVEL'
      siteUrl<-linkGeral[[pg]]
      print(siteUrl)
      
      pg<-pg+1
      
      Sys.sleep(4)},
      error = function(cond){
        print(cond)
      })
    
    
  }else{
    tryCatch({
      tryCatch({
        nomeProduto<-remDr$findElements('class name','vtex-store-components-3-x-productBrand--quickview')
        nomeProduto<-nomeProduto[[1]]$getElementText()
        print(nomeProduto)},
        error=function(cond){
          print(cond)
        })
      
      tryCatch({
        precoProduto<-remDr$findElements('class name','vtex-product-price-1-x-sellingPriceValue')
        precoProduto<-precoProduto[[1]]$getElementText()
        print(precoProduto)},
        error = function(cond){
          print(cond)
        })
      
      
      tryCatch({
        codProduto<-remDr$findElements('class name','vtex-refid')
        codProduto<-codProduto[[1]]$getElementText()
        codProduto<-substr(codProduto,6,30)
        print(codProduto)},
        error = function(cond){
          print(cond)
        })
      
      
      linhaProduto<-ifelse(minerador==1,linha[[1]],
                           ifelse(minerador==2, linha[[2]],
                                  ifelse(minerador==3, linha[[3]],
                                         ifelse(minerador==4, linha[[4]],linha[[5]]))))
      print(linhaProduto)
      
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
        listaImagem<-remDr$findElements('class name','vtex-store-components-3-x-productImageTag')
        urlImagem<-listaImagem[[1]]$getElementAttribute('src')
        print(urlImagem)},
        error = function(cond){
          print(cond)
        })
      
      siteUrl<-linkGeral[[pg]]
      print(siteUrl)
      
      pg<-pg+1
      
      if(length(precoProduto)==0){
        precoProduto<-"Produto Indisponível"
        print(precoProduto)
      }else{
        print(precoProduto)
      }
      
      
      dataMineracao<-Sys.Date()
      
      #REMOVER ACENTOS 
      nomeProduto<-chartr('áéíóÁÉÍÓÂÊÎÔâêîôãõÃÕçÇÀà','aeioAEIOAEIOaeioaoAOcCAa',nomeProduto)
      #REMOVER APOSTROFO
      nomeProduto<-str_replace_all(nomeProduto,"[']"," ")
    
      
      #CRIANDO TABELA DE DADOS 
      tabelaDados<-rbind(tabelaDados, cbind(dataMineracao, Cliente, nomeProduto,linhaProduto, volumeProduto,precoProduto, codProduto, urlImagem, siteUrl))
      
      Sys.sleep(4)
    },
    error=function(cond){
      print(cond)
    })
    
  }
}
  },
  error =function(cond){
    print("Pagina finaliza,dando continuidade as minerações")
  })




minerador<-minerador+1
}

remDr$close() #Fecho o navegador
rD$server$stop() #Fecha conexão realizada com servidor 
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE) #Forçando o R a fechar todas as portas Java (se necessário)
rm(rD, remDr) # excluindo os dados no final 
gc() #limpeza de disco
