library(ggplot2)
library(tidyverse)
library(xlsx)
library(rgdal)
library(plyr)
library(rgeos)
library(sf)
library(gganimate)
library(transformr)
library(scales)
library(png)
library(ggsflabel)

# Organizacao das bases
organiza_base_estados <- function(base,variavel_separada){
  nomes <- names(base)
  nomes[match(variavel_separada,nomes)] <- "Data"
  cat(names(base[,1]))
  names(base) <- nomes
  
  base <- base %>%
    gather(key = Estados,value = Valores,-Data) %>%
    mutate(Estados = toupper(Estados)) %>%
    mutate(Estados = gsub(pattern = "\\.",replacement = " ",x = Estados)) %>%
    mutate(Estados = gsub("Ă","Ã",Estados))
  return(base)
}

# Gerar mapa base
gerador_base_mapa <- function(estados,batimento){
  load("mapa_base")
  if(exists("batimento")==T){
    batimento <- tolower(batimento)
    if(!(batimento %in% c("estados","sigla"))){
      stop("OPÇÕES DE BATIMENTO NÃO POSSÍVEIS")
    }
  }
  if(exists("batimento")==F){
    stop("APRESENTE UMA OPÇÃO DE BATIMENTO")
  }
  # Se a escolha for estado
  if(batimento == "estados"){
    names(estados) <- c("Data","NM_ESTADO","Valores")
    base <- merge(x = mapa_base, y = estados, by = "NM_ESTADO",all.x=T)
    itens_faltantes <- mapa_base$NM_ESTADO %in% base$NM_ESTADO
  }
  if(batimento == "siglas"){
    names(estados) <- c("Data","NM_SIGLAS","Valores")
    base <- merge(x = mapa_base, y = estados, by = "NM_SIGLAS")
    itens_faltantes <-base$NM_ESTADO %in% mapa_base$NM_ESTADO
  }
  cat(NROW(base)/27, " Observações\n")
  statistica <- count(is.na(base$Valores))
  cat(statistica$freq[1]," observações NA são",statistica$x[1], "\n")
  cat(statistica$freq[2]," observações NA são",statistica$x[2], "\n")
  return(base)
  
}


