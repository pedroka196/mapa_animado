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
  nomes <- iconv(nomes,to="ASCII//TRANSLIT")
  nomes[match(variavel_separada,nomes)] <- "Data"
  cat(names(base[,1]))
  names(base) <- nomes
  
  base <- base %>%
    gather(key = Estados,value = Valores,-Data) %>%
    mutate(Estados = toupper(Estados)) %>%
    mutate(Estados = gsub(pattern = "\\.",replacement = " ",x = Estados)) %>%
    mutate(Estados = gsub("Ă","A",Estados))
  return(base)
}

# Gerar mapa base
gerador_base_mapa <- function(estados,batimento){
  load("mapa_base")
  if(exists("batimento")==T){
    batimento <- tolower(batimento)
    if(!(batimento %in% c("estados","siglas"))){
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
    # itens_faltantes <- mapa_base$NM_ESTADO %in% base$NM_ESTADO
  }
  if(batimento == "siglas"){
    names(estados) <- c("Data","NM_SIGLAS","Valores")
    base <- merge(x = mapa_base, y = estados, by = "NM_SIGLAS",all.x=T)
    # itens_faltantes <-base$NM_SIGLAS %in% mapa_base$NM_SIGLAS
  }
  # cat(NROW(base)/27, " Observações\n")
  # statistica <- count(is.na(base$Valores))
  # cat(statistica$freq[1]," observações NA são",statistica$x[1], "\n")
  # cat(statistica$freq[2]," observações NA são",statistica$x[2], "\n")
  return(base)
  
}


gerador_mapa_visual <- function(base,variavel,nome,fonte,tipo_escala){
  
  
  grafico_mapa <- base %>%
    # filter(Data == 2000) %>%
    ggplot(aes(group = Data)) #+
  if(missing(variavel)){
    variavel = "Valores"
    count(unique(base[variavel]))
  }
  discreto <- as.data.frame(base) %>%
    select(variavel)
   
  discreto.count <- nrow(unique(discreto))
    grafico_mapa <- grafico_mapa +
      geom_sf(aes_string(fill=variavel,frame = "Data"),data = base)+
      geom_sf_text(aes(label = round(Valores,digits = 1)),fontface = "bold")
  
  grafico_mapa <- grafico_mapa+
    ggtitle(nome)+
    labs(caption = fonte,
         subtitle = "Ano: {closest_state}")+
    theme_minimal()
  if(tipo_escala == "continua"){
    grafico_mapa <- grafico_mapa + scale_fill_gradient(low = grDevices::heat.colors(17)[15],
                                                       high = grDevices::heat.colors(17)[1],
                                                       guide = guide_colorbar(frame.colour = "#1b1b1b",ticks.colour = "#1b1b1b"))
  }
  if(tipo_escala == "discreta"){
    # grafico_mapa <-
      # grafico_mapa + scale_color_discrete_sequential(grDevices::heat.colors(discreto.count))
  }

    
  
  tema <- theme(#line = element_line(color = "#1b1b1b"),
    # line = element_blank(),
    # axis.line.x = element_line(),
    # axis.line.y = element_line(),
    title = element_text(family = "Calibri",size = 14,hjust=0.5,face = "bold"),
    plot.margin = unit(c(0.5,1,0.5,0.5),"cm"),
    plot.subtitle = element_text(family = "Calibri",hjust=0.5,size = 17,face = "bold"),
    plot.caption = element_text(family = "Calibri",hjust=0.5,size = 10,face = "bold"),
    legend.key = element_blank(),
    legend.title = element_blank(),
    panel.background  = element_rect(fill = "#4ea1c6"),
    panel.grid = element_line(colour = "#aed5e7",size = 0.3),
    panel.border = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank())
  
  
  return(grafico_mapa+tema)
  
}

gerador_animacao <- function(grafico,frames_per_sec,duracao,horizontal,vertical,pausa_inicial,pausa_final){
  # Se faltar informacoes
  if(missing(vertical)){
    vertical = 1000
  }
  if(missing(horizontal)){
    horizontal = 1000
  }
  if(missing(pausa_inicial)){
    pausa_inicial = 0
  }
  if(missing(pausa_final)){
    pausa_final = 0
  }
  if(missing(duracao)){
    duracao = 10
  }
  if(missing(frames_per_sec)){
    frames_per_sec = 12
  }
  if(missing(grafico)){
    stop("NÃO TEM GRÁFICO")
  }
  
  
  
  cat("antes do grafico mapa")
  grafico_mapa = grafico + transition_states(Data)
  cat("depois do grafico mapa")
  # animate(grafico_mapa, fps = frames_per_sec,duration = duracao)
  grafico_mapa_animado <- animate(grafico_mapa,
                                  fps = frames_per_sec,
                                  duration = duracao,
                                  height = vertical,
                                  width = horizontal,
                                  start_pause = pausa_inicial,
                                  end_pause = pausa_final)
  
  anim_save(filename = "mapa01.gif",animation = grafico_mapa_animado)
  
  return(grafico_mapa_animado)
}

teste_animado <- gerador_animacao(grafico = mapa_homicidios)
