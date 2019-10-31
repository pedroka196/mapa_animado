library(data.table)
library(tidyverse)
# proposicoes_camara

# Padrão de download
propsicoes <- "https://dadosabertos.camara.leg.br/arquivos/proposicoesAutores/csv/proposicoesAutores-"

# período desejado e tipo de dado a ser baixado
anos_desejados <- 1988:2018
tipo_dados <- "csv"

# cria destino
dir.create("proposicoes")

# loop de download
for(i in anos_desejados){
  url_usada <- paste0(propsicoes,i,".",tipo_dados)
  download.file(url = url_usada,destfile = paste0("proposicoes/proposicoes",i,".csv"))
}


# Loop criação base
for(i in anos_desejados){
  if(i == anos_desejados[1]){
    proposicoes <- as.data.frame(matrix(0,nrow = 0,ncol = 3))
    names(proposicoes) <- c("Data","Estados","Valores")
    proposicoes$Estados <- as.factor(proposicoes$Estados)
  }
  temp <- fread(paste0("proposicoes/proposicoes",i,".csv"))
  
  temp <- as.data.frame(temp)
  temp <- temp %>%
    filter(codTipoAutor == 10000, siglaUFAutor != "") %>%
    mutate(siglaUFAutor = as.factor(siglaUFAutor),
           siglaPartidoAutor = as.factor(siglaPartidoAutor)) %>%
    group_by(siglaUFAutor) %>%
    summarise(proponentes = dplyr::n(),ano=i) %>%
    ungroup(siglaUFAutor)
  
  
  names(temp) <- c("Estados","Valores","Data") 
  temp <- temp[,c(3,1,2)]
  
  proposicoes <- rbind.data.frame(proposicoes,temp)
}


mapa <- gerador_base_mapa(estados = proposicoes,batimento = "siglas")

mapa_img <- gerador_mapa_visual(base = mapa,nome = "Quantidade de proposições de deputados por estado por ano",fonte = "Fonte: Dados Abertos - Câmara dos Deputados",tipo_escala = "continua",variavel = "Valores")


mapa_anim <- gerador_animacao(grafico = mapa_img,frames_per_sec = 12,duracao = 20,horizontal = 1000,vertical = 1000,pausa_inicial = 1,pausa_final = 1)

save_animation(mapa_anim,file = "proposicoes.gif")
