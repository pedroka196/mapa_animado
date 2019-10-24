library(ggplot2)
library(tidyverse)
library(xlsx)
library(rgdal)
library(plyr)
library(rgeos)
library(utf8)
library(sf)
library(gganimate)
library(transformr)
library(scales)
library(png)
library(ggsflabel)

source("funcoes.R")
# library(broom)
# require("rgdal") # requires sp, will use proj.4 if installed
# require("maptools")
# require("plyr")

organiza_base_estados <- function(base,variavel_separada){
  nomes <- names(base)
  nomes[match(variavel_separada,nomes)] <- "Data"
  cat(names(base[,1]))
  names(base) <- nomes
  
  base <- base %>%
    gather(key = Estados,value = Valores,-Data) %>%
    mutate(Estados = toupper(Estados)) %>%
    mutate(Estados = gsub(pattern = "\\.",replacement = " ",x = Estados))
  return(base)
}

dados <- organiza_base_estados(homicidios_base2,"Anos")

# plotar gráficos do brasil
homicidios_base2 <- read.xlsx("homicidios.xlsx",sheetIndex = 2,encoding = "UTF-8")

base_homicidio <- organiza_base_estados(homicidios_base2,variavel_separada = "Anos")
base_homicidio <- gerador_base_mapa(estados = base_homicidio,batimento = "estados")
mapa_homicidios <- gerador_mapa_visual(base = base_homicidio,
                                       nome = "Taxa de homicídios de 1981 a 2017",
                                       fonte = "IpeaData",
                                       tipo_escala = "discreta",
                                       variavel = "Valores")

mapa_homicidios_animado <- gerador_animacao(grafico = mapa_homicidios,duracao = 10)

# Espírito santo dá erro
homicidios_base$NM_ESTADO = ifelse(homicidios_base$NM_ESTADO=="ESPÍRITO SANTO","ESPIRITO SANTO",homicidios_base$NM_ESTADO)

# mapa_base <- readOGR(dsn = "brasil/",layer = "UFEBRASIL", stringsAsFactors = F)
# # Estrutura do shapefile
# glimpse(mapa_base)
# 
# # Simplificar shapefile
# mapa_base <- rgeos::gSimplify(spgeom = mapa_base, tol = 0.1)
# 
# # Tidy remove o @data
# mapa_base@data <- merge(x = mapa_base@data, y = homicidios_base, by = "NM_ESTADO")
# mapa_base_dados <- fortify(mapa_base)
# 
# mapa_base@data$id <- 0:(dim(mapa_base@data)[1]-1)
# 
# # Junta mapas pelo ID
# mapa_base_dados <- join(x = mapa_base_dados,y = mapa_base@data,by = "id")
# 
# grafico_mapa <- mapa_base_dados %>%
#   filter(Anos == 2015) %>%
#   group_by(NM_ESTADO) %>%
#   ggplot() +
#   geom_polygon(aes(x = long, y = lat, group = NM_ESTADO, fill = TaxaHomicidio),
#                colour = "black", size = 0.5)

mapa_base <- st_read("brasil",layer = "UFEBRASIL") %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  st_simplify(dTolerance = 0.1)

NM_SIGLAS <- c("DF","GO","MT","MS",
               "AL","BA","CE","MA","PB","PE","PI","RN","SE",
               "AC","AP","AM","PA","RO","RR","TO",
               "ES","MG","SP","RJ",
               "PR","RS","SC")


mapa_base <- cbind2(x= arrange(df = mapa_base,NM_REGIAO,NM_ESTADO),y=NM_SIGLAS)
  merge(x= arrange(df = mapa_base,NM_REGIAO,NM_ESTADO),y=NM_SIGLAS)
names(mapa_base) <- c("ID","CD_GEOCODU","NM_ESTADO","NM_REGIAO","NM_SIGLAS","geometry")

mapa_base <- merge(x = mapa_base, y = homicidios_base, by = "NM_ESTADO")
mapa_base$NM_ESTADO <- sub("ESPIRITO","ESPÍRITO",mapa_base$NM_ESTADO)


# plota tudo
plot(mapa_base)
save(mapa_base,file = "mapa_base")

# mapa_base$TaxaHomicidio <- mapa_base$TaxaHomicidio/100


grafico_mapa <- base %>%
  filter(Data == 2000) %>%
  ggplot(aes(group = Data)) +
  geom_sf(aes(fill=Valores,frame = Data),data = base)+
  # geom_sf_text_repel(aes(label = TaxaHomicidio))+
  geom_sf_text(aes(label = round(Valores,digits = 1)),fontface = "bold")+
  # geom_text_repel(aes(label = scales::percent(TaxaHomicidio,suffix = "%"),))+
  ggtitle("Taxa de homicídios por 100 mil habitantes")+
  labs(caption = "Fonte: Ipeadata. Elaboração: Pedro Henrique Oliveira",
       subtitle = "Ano: {closest_state}")+
  theme_minimal()+
  scale_fill_viridis_c(option = "inferno",
                       values = c(0,2),
                       end = 0,
                       begin = 1,
                       limits = c(0,100),
                       aesthetics = c("color","fill"),
                       guide = guide_colorbar(frame.colour = "#1b1b1b",ticks.colour = "#1b1b1b"))
  # scale_fill_grey(start = 0.5,end = 1,aesthetics = 1)
  # scale_fill_gradient(aes(fill=TaxaHomicidio),
  #                     low = "#ffdddd",
  #                     high = "#a90000"#,
  #                     # limits = c(0,1)#,
  #                     # label = percent,
  #                     # guide = guide_colorbar(frame.colour = "#1b1b1b",ticks.colour = "#1b1b1b")
  #                     )

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

grafico_mapa + tema


grafico_mapa = grafico_mapa + 
  tema + 
  transition_states(Anos)
animate(grafico_mapa, fps = 3,duration = 5)
grafico_mapa_animado <- animate(grafico_mapa,fps = 3,duration = 30,height = 600,width = 800,start_pause = 2,end_pause = 2)

anim_save(filename = "mapa01.gif",animation = grafico_mapa_animado)


### Producao amendoim
amendoim <- read.xlsx("ProducaoAmendoim.xlsx",sheetIndex = 2,encoding = "UTF-8")
amendoim <- amendoim %>%
  gather(key = Estados,value = ProducaoAmendoim,-Anos) %>%
  mutate(Estados = toupper(Estados)) %>%
  mutate(Estados = gsub(pattern = "\\.",replacement = " ",x = Estados)) #%>%
  mutate(Estados = gsub("Ă","Ã",Estados))

names(amendoim) <- c("Anos","NM_ESTADO","ProducaoAmendoim")

load("mapa_base")

mapa_base <- merge(x=mapa_base,y = amendoim,by = "NM_ESTADO")



grafico_mapa <- dados_mapa %>%
  # filter(Data == 2000) %>%
  ggplot(aes(group = Data)) +
  geom_sf(aes_string(fill="NM_ESTADO",frame="Data"))+
  # geom_sf_text_repel(aes(label = TaxaHomicidio))+
  geom_sf_text(aes(label = round(Valores,digits = 1)),fontface = "bold")+

  grafico_mapa <- mapa_base %>%
  filter(Anos == as.Date.character("01-01-2004","%d-%m-%Y")) %>%
  ggplot(aes(group = Anos)) +
  geom_sf(aes(fill=ProducaoAmendoim,frame = Anos))+
  # geom_sf_text_repel(aes(label = TaxaHomicidio))+
  geom_sf_text(aes(label = round(ProducaoAmendoim,digits = 1)),fontface = "bold")+
  # geom_text_repel(aes(label = scales::percent(TaxaHomicidio,suffix = "%"),))+
  ggtitle("Produção anual de amendoim com casca\n em toneladas")+
  labs(caption = "Fonte: Ipeadata. Elaboração: Pedro Henrique Oliveira",
       subtitle = "Ano: {closest_state}")+

  theme_minimal()
  # scale_fill_viridis_c(option = "inferno",
                       # values = c(0,2),
                       # end = 0,
                       # begin = 1,
                       # aesthetics = c("color","fill"),
  #                      # guide = guide_colorbar(frame.colour = "#1b1b1b",ticks.colour = "#1b1b1b"))
  scale_fill_viridis_d()
  theme_minimal()+
  scale_fill_viridis_c(option = "inferno",
                       values = c(0,2),
                       end = 0,
                       begin = 1,
                       aesthetics = c("color","fill"),
                       guide = guide_colorbar(frame.colour = "#1b1b1b",ticks.colour = "#1b1b1b"))

grafico_mapa+tema
load("mapa_base")
