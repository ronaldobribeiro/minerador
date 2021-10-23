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

Cliente = 'NATURA'
#ABRINDO NAVEGADOR 
rD<-rsDriver(port = 4444L, browser = c('chrome'),chromever = '94.0.4606.61',verbose = TRUE, check = TRUE) #Codigo que iremos passar o parametros para configurar o servidor
remDr<-rD[['client']] #Lista gerada, que é tipica de um cliente mexer no navegador
remDr$maxWindowSize()
remDr$navigate('https://www.natura.com.br/c/tudo-em-corpo-e-banho') #Navigate é uma função, para abrir o site
Sys.sleep(5)

#ABRINDO TODAS AS PAGINAS EXISTENTES
tryCatch({
  for (p in 1:50){
    webElem<-remDr$findElements('tag name','body')[[1]]
    webElem$sendKeysToElement(list(key='end'))
    Sys.sleep(2)
    
    for (i in 1:15){
      webElem$sendKeysToElement(list(key='up_arrow'))
    }
    
    botaopagina<-remDr$findElements('class name', 'ProductList_loadMore__WlWYt')
    botaopagina[[1]]$clickElement()
    Sys.sleep(2)
  }
},
error = function(cond){
  print('não há mais paginas nesta tela')
  print(cond)
})


#GERANDO TODOS OS LINKS DE ACESSO
linkProduto<-remDr$findElements('class name', 'gtm-product')
aux<-1
qtd<-length(linkProduto)
for(n in aux:qtd+1){
  link<-linkProduto[[aux]]$getElementAttribute('href')
  linkGeral<-c(linkGeral,link)
  aux<-aux+1
  print(link)
}

#CRIANDO TABELA COM TODOS OS DADOS
aux<-2
tryCatch({
  for(p in 1:1000){
    remDr$navigate(linkGeral[[aux]])
    
    nomeProduto<-remDr$findElements('class name', paste0('MuiTypography-alignLeft'))
    nomeProduto<-nomeProduto[[1]]$getElementText() 
    nomeProduto<-nomeProduto[[1]] 
    print(nomeProduto)
    
    precoProduto<-remDr$findElements('class name', 'Pricing_product-pricing__9huBK')
    precoProduto<-precoProduto[[1]]$getElementText()
    precoProduto<-precoProduto[[1]]
    
    
    precoProduto<-if(str_count(precoProduto) >8){
      str_sub(precoProduto,start =10)
    }else {
      precoProduto
    }
    print(precoProduto)
    
    codProduto<-remDr$findElements('xpath', '//*[@id="sticky-observable"]/div[2]/div[2]/p')
    codProduto<-codProduto[[1]]$getElementText()
    valor = ''
    estado = 1
    x=1
    
    for(c in 1:100){
      if(estado == 1){
        if(str_sub(codProduto,start = x, end = x) != " "){
          estado=1
          x=x+1
        }else if(str_sub(codProduto,start = x, end = x) ==" "){
          estado=2
          x=x+1
        }
      }else if(estado ==2){
        if(str_sub(codProduto,start = x, end = x) != ' '){
          valor = paste0(valor,str_sub(codProduto,start = x, end = x))
          estado=2
          x=x+1
        }else if(c == ' '){
          print(valor)
        }
      }
    }
    codProduto<-valor
    print(codProduto)
    
    
    linhaProduto<-remDr$findElements('xpath','//*[@id="sticky-observable"]/div[2]/div[1]/h2')
    linhaProduto<-linhaProduto[[1]]$getElementText()
    linhaProduto<-linhaProduto[[1]]
    print(linhaProduto)
    
    siteUrl<-linkGeral[[aux]]
    print(siteUrl)
    aux<-aux+2
    Sys.sleep(5)
    
    tryCatch({
      volumeProduto<-remDr$findElements('xpath', '//*[@id="sticky-observable"]/div[2]/div[2]/p')
      volumeProduto<-volumeProduto[[1]]$getElementText()
      volumeProduto<-volumeProduto[[1]]
      #FUNCAO DE EXTRACAO 4 ULTIMOS DIGITOS
      substrRight <- function(x, n){
        substr(x, nchar(x)-n+1, nchar(x))
      }
      volumeProduto<-substrRight(volumeProduto,10)
    },error = function(cond){
      volumeProduto<-'0'
      print(cond)
    })
    
    #LIMPANDO CARACTERES ESPECIAIS 
    volumeProduto<-iconv(volumeProduto, from = 'UTF-8', to = 'ASCII//TRANSLIT')
    volumeProduto<-gsub('[a-z -][A-Z]*','',volumeProduto)
    volumeProduto<-gsub("[[:punct:]]",'',volumeProduto)
    
    volumeProduto<-if(str_detect(siteUrl,pattern = 'ml')==TRUE){
      paste(volumeProduto,'ml')
    }else if(nchar(volumeProduto)==0){
      '0'
    }else{
      paste(volumeProduto,'g')
    }
    print(volumeProduto)
    
    listaImagem <-remDr$findElements('tag name', 'img')
    urlImagem <-listaImagem[[1]]$getElementAttribute('src')
    urlImagem<- urlImagem[[1]]
    print(urlImagem)
    
    
    dataMineracao<-Sys.Date()
    tabelaDados<-rbind(tabelaDados, cbind(dataMineracao, Cliente, nomeProduto,linhaProduto, volumeProduto,precoProduto, codProduto, urlImagem, siteUrl))
    
  }
}, 
error = function(cond){
  print('Mineração finalizada')
  print(cond)
})


remDr$close() #Fecho o navegador
rD$server$stop() #Fecha conexão realizada com servidor 
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE) #Forçando o R a fechar todas as portas Java (se necessário)
rm(rD, remDr) # excluindo os dados no final 
gc() #limpeza de disco




