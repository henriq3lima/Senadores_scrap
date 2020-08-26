#Senadores Br
library(tidyverse)
library(xml2)
library(rvest)
library(httr)
library(chron)

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
senadores$UF[!is.na(senadores$UF)]




