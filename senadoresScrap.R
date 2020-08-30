#Senadores Br
library(tidyverse)
library(xml2)
library(rvest)
library(httr)
library(chron)
rm(list = ls())
url <- 'https://www25.senado.leg.br/web/transparencia/sen/outras-legislaturas'
page_html <-  read_html(url) 
senadores <- (html_nodes(page_html, '#senadoreslegislaturasanteriores-tabela-senadores') %>% html_table(fill = TRUE))[[1]]
senadores$Nome[senadores$Nome %>% str_detect(' - ')] <- NA
senadores <- dplyr::filter(senadores,!is.na(Nome))
#Senador(a) suplente que entrou em exercício durante a legislatura
senadores$suplente[senadores$Nome %>% str_detect('[*]')] <- 1
for (i in 1:nrow(senadores)) {
  if(is.na(senadores$UF[i])) senadores$UF[i] <- senadores$UF[i-1] 
}
for (i in 1:nrow(senadores)) {
  if(is.na(senadores$Período[i])) senadores$Período[i] <- senadores$Período[i-1] 
}
senadores$suplente[is.na(senadores$suplente)] <- 0
senadores$Nome <- str_remove_all(senadores$Nome,'[*]')

i=1
#Pesquisa google bing
dataGooglenews <- function(pesquisa){
  sessao <- html_session('google.com.br')
  form_null <- html_session('google.com.br') %>% html_form()
  form_null[[1]]$fields[[6]]$value <- pesquisa #form_pesq <- set_values(form_null[[1]],'q' = pesquisa)
  page_html <- (submit_form(sessao,form_null[[1]]))[[3]] %>% read_html() 
  v1 <- page_html %>% html_nodes(xpath = '//div[@id="main"]/div[1]/div[1]/div[1]/div[1]/div/a[1]') %>%
    html_attr('href')
  v1 <- str_c('https://www.google.com',v1)
  page_html <- read_html(v1)
  noticias <- page_html %>% xml_find_all('//div[@class="BNeawe vvjwJb AP7Wnd"]') %>% html_text()
  nextPage <- page_html %>% html_nodes(xpath = '//a[@class="nBDE1b G5eFlf"]') %>% html_attr('href')
  while (length(nextPage)!=0) {
    page_html <- str_c('https://www.google.com',nextPage) %>%  read_html()
    if (str_detect(v1,'sorry')) { return('robot')  }
    noticias <- c(noticias,xml_find_all(page_html,'//div[@class="BNeawe vvjwJb AP7Wnd"]') %>% html_text() )
    nextPage <- (page_html %>% html_nodes(xpath = '//a[@class="nBDE1b G5eFlf"]') %>% html_attr('href'))[length((page_html %>% html_nodes(xpath = '//a[@class="nBDE1b G5eFlf"]') %>% html_attr('href')))]
    espera <- runif(1,10.5,27) 
    Sys.sleep(espera) 
  }
  return(noticias)
}
google_shearch <- function(busca,inic=1){
  for (i in inic:length(busca)) {
    x <- length(busca)
    if (length(busca)==1){
      noticias <- dataGooglenews(busca[i])
      noticias <- data.frame(Nome = rep(busca[i],length(noticias)),g1.com = noticias) 
      return(noticias)
    }
    if (i == 1) {
      noticias <- dataGooglenews(busca[i])
      noticias <- data.frame(Nome = rep(busca[i],length(noticias)),g1.com = noticias) 
      i=i+1
    }
    espera <- runif(1,5.3,50) 
    Sys.sleep(espera)
    aux <- dataGooglenews(busca[i])
    if(length(aux)==0) aux <- NA
    if (!is.na(aux)) {
     }
    noticias <- rbind(noticias,data.frame(
      Nome = rep(busca[i],length(aux)),
      g1.com = aux))
  }
  return(noticias)
}
rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  pattern <- unique(pattern)
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  accentTypes <- c("´","`","^","~","¨","ç")
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  return(str)
}



busca <- rm_accent(senadores$Nome)
busca <- str_c('site:g1.globo.com intitle:',busca,' reu')



a <- list()
for (i in 1:length(busca)) {
  a[[i]] <- google_shearch(busca[i])

}

for (i in 27:length(busca)) {
  print(i)
  a[[i]] <- google_shearch(busca[i])
  
}

not <- rbind_list(a)
setwd("~/GitHub/Senadores_scrap")
#save.image("~/GitHub/Senadores_scrap/Workspace.RData")
#load("~/GitHub/Senadores_scrap/Workspace.RData")


bing <- 'www.bing.com/search?q='










