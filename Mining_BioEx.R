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

Cliente = 'BIOEXTRATUS'
#ABRINDO NAVEGADOR 
rD<-rsDriver(port = 4444L, browser = c('chrome'),chromever = '94.0.4606.61',verbose = TRUE, check = TRUE) #Codigo que iremos passar o parametros para configurar o servidor
remDr<-rD[['client']] #Lista gerada, que é tipica de um cliente mexer no navegador
remDr$maxWindowSize()


pagina<-c('https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/-brilho','https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/-liso',
          'https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/anticaspa','https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/blond-bioreflex',
          'https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/botica-algas','https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/botica-arnica',
          'https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/botica-cachos','https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/botica-camomila',
          'https://lojabusca.bioextratus.com.br/p/coloracao-todos-produtos/','https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/botica-henna',
          'https://lojabusca.bioextratus.com.br/p/cuidado-e-protecao/f/linhas/botica-lavanda','https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/cabelo-e-barba',
          'https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/cachos--crespos',
          'https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/forca','https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/homem-classica',
          'https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/jaborandi','https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/kids-menino-maluquinho',
          'https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/mel','https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/neutro',
          'https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/nutri-cachos','https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/pos-coloracao',
          'https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/pos-quimica','https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/queravit',
          'https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/shitake-plus','https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/specialiste-colorante',
          'https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/specialiste-detox','https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/specialiste-matizante',
          'https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/specialiste-resgate','https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/tratamentos-complementares',
          'https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/tutano','https://lojabusca.bioextratus.com.br/p/tratamento-capilar/f/linhas/umectante')

linha<-c('BRILHO', '+LISO','ANTICASPA', 'BIOREFLEX', 'ALGAS','ARNICA','CACHOS','CAMOMILA', 'COLORAÇÃO','HENNA', 'LAVANDA','CABELO E BARBA','CRESPOS','MICHEL MERCIER','FORCA','HOMEM CLASSICA','JABORANDI','KIDS MENINO MALUQUINHO',
         'MEL', 'NEUTRO', 'NUTRI CACHOS', 'POS COLORACAO','POS QUIMICA', 'QUERAVIT', 'SHITAKE PLUS', 'SPECIALISTE COLORANTE', 'SPECIALISTE DETOX','SPECIALISTE MATIZANTE','SPECIALISTE RESGATE', 'TRATAMENTOS COMPLEMENTARES','TUTANO',
         'UMECTANTE')


minerador=1
for (m in 1:33){
linkGeral <- data.frame(link=as.character(character()),
                          stringsAsFactors=FALSE) 

remDr$navigate(pagina[[minerador]])
Sys.sleep(4)


webElem<-remDr$findElements('tag name','body')[[1]]
webElem$sendKeysToElement(list(key='end'))

for (p in 1:8){
  webElem$sendKeysToElement(list(key='up_arrow'))
}

linkProduto<-remDr$findElements('class name','product-name')
aux<-1
qtd<-length(linkProduto)
for (c in aux:qtd+1){
  link<-linkProduto[[aux]]$getElementAttribute('href')
  linkGeral<-c(linkGeral, link)
  aux=aux+1
  print(linkGeral)
  }

pp<-3
tryCatch({
if(length(remDr$findElements('class name','biggy-pagination__item'))>0){
  for (t in 1:10) {
    remDr$findElements('class name','biggy-pagination__item')[[pp]]$clickElement()
    Sys.sleep(2)
    aux<-1
    linkProduto<-remDr$findElements('class name','product-name')
    for (n in aux:length(linkProduto)){
      link<-linkProduto[[aux]]$getElementAttribute('href')
      linkGeral<-c(linkGeral, link)
      aux=aux+1
      print(linkGeral)
    }
    pp<-pp+1
  }
}else{
  print('iniciar extração de dados')
}
}, error = function(cond){
  print('Iniciar extração dos dados')
})

#PEGANDO TODOS OS LINKS DAS PAGINAS 
aux_cod<-1
pg<-3 
qtd<-length(linkGeral)

tryCatch({
for (x in 1:qtd+1){
remDr$navigate(linkGeral[[pg]])


tryCatch({
nomeProduto<-remDr$findElements('class name','fn')
nomeProduto<-nomeProduto[[1]]$getElementText()
print(nomeProduto)},
error = function(cond){
  print(cond)
})

tryCatch({
precoProduto<-remDr$findElements('class name','skuBestPrice')
precoProduto<-precoProduto[[1]]$getElementText()
print(precoProduto)},
error=function(cond){
  print(cond)
})



codProduto<-paste0('BIO',aux_cod)
print(codProduto)
aux_cod<-aux_cod+1
linhaProduto<-ifelse(minerador==1,linha[[1]],
                     ifelse(minerador==2, linha[[2]],
                     ifelse(minerador==3, linha[[3]],
                     ifelse(minerador==4, linha[[4]],
                     ifelse(minerador==5, linha[[5]],
                     ifelse(minerador==6, linha[[6]],
                     ifelse(minerador==7, linha[[7]],
                     ifelse(minerador==8, linha[[8]],
                     ifelse(minerador==9, linha[[9]],
                     ifelse(minerador==10, linha[[10]],
                     ifelse(minerador==11, linha[[11]],
                     ifelse(minerador==12, linha[[12]],
                     ifelse(minerador==13, linha[[13]],
                     ifelse(minerador==14, linha[[14]],
                     ifelse(minerador==15, linha[[15]],
                     ifelse(minerador==16, linha[[16]],
                     ifelse(minerador==17, linha[[17]],
                     ifelse(minerador==18, linha[[18]],
                     ifelse(minerador==19, linha[[19]],
                     ifelse(minerador==20, linha[[20]],                                                                                                                              
                     ifelse(minerador==21, linha[[21]],                     
                     ifelse(minerador==22, linha[[22]],
                     ifelse(minerador==23, linha[[23]],        
                     ifelse(minerador==24, linha[[24]],       
                     ifelse(minerador==25, linha[[25]],                           
                     ifelse(minerador==26, linha[[26]],                           
                     ifelse(minerador==27, linha[[27]],
                     ifelse(minerador==28, linha[[28]],
                     ifelse(minerador==29, linha[[29]],
                     ifelse(minerador==30, linha[[30]],
                     ifelse(minerador==31, linha[[31]],linha[[32]])))))))))))))))))))))))))))))))

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
  listaImagem<-remDr$findElements('id','image-main')
  urlImagem<-listaImagem[[1]]$getElementAttribute('src')
  print(urlImagem)},
  error = function(cond){
    print(cond)
  })

siteUrl<-linkGeral[[pg]]
print(siteUrl)

pg<-pg+1
Sys.sleep(2)



dataMineracao<-Sys.Date()
#CRIANDO TABELA DE DADOS 
tabelaDados<-rbind(tabelaDados, cbind(dataMineracao, Cliente, nomeProduto,linhaProduto, volumeProduto,precoProduto, codProduto, urlImagem, siteUrl))



}
},error = function(cond){
  print('Sem dados para minerar nesta pagina, iniciar proxima.')
})


minerador<-minerador+1

}


remDr$close() #Fecho o navegador
rD$server$stop() #Fecha conexão realizada com servidor 
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE) #Forçando o R a fechar todas as portas Java (se necessário)
rm(rD, remDr) # excluindo os dados no final 
gc() #limpeza de disco
