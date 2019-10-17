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
# library(broom)
# require("rgdal") # requires sp, will use proj.4 if installed
# require("maptools")
# require("plyr")


# plotar gráficos do brasil
base_dcl_rcl <- read.xlsx("DCL_RCL.xlsx",sheetIndex = 1)
base_dcl_rcl <- base_dcl_rcl %>%
  gather(key = Estados,value = Relacao_DCL_RCL,-DCL_RCL) %>%
  mutate(Estados = toupper(Estados)) %>%
  mutate(Estados = gsub(pattern = "\\.",replacement = " ",x = Estados)) %>%
  mutate(Estados = gsub("Ă","Ã",Estados)) %>%
  mutate(Relacao_DCL_RCL = as.numeric(Relacao_DCL_RCL)) %>%
  mutate(perfil_endiv = ifelse(Relacao_DCL_RCL < 50,"Baixo",NA)) %>%
  mutate(perfil_endiv = ifelse(Relacao_DCL_RCL >= 50 & Relacao_DCL_RCL < 100, "Intermediário",perfil_endiv)) %>%
  mutate(perfil_endiv = ifelse(Relacao_DCL_RCL >= 100 & Relacao_DCL_RCL < 200, "Alto",perfil_endiv)) %>%
  mutate(perfil_endiv = ifelse(Relacao_DCL_RCL > 200, "Acima do limite",perfil_endiv)) %>%
  mutate(perfil_endiv = factor(x = perfil_endiv,levels = c("Baixo","Intermediário","Alto","Acima do limite")))


names(base_dcl_rcl) <- c("Anos","NM_ESTADO","DCL_RCL","Perfil_Endividamento")

base_dcl_rcl$Anos <- as.character(base_dcl_rcl$Anos)
base_dcl_rcl$Anos <- ifelse(base_dcl_rcl$Anos == "2018¹",2018,base_dcl_rcl$Anos)
base_dcl_rcl$Anos <- as.numeric(base_dcl_rcl$Anos)
# Espírito santo dá erro
# homicidios_base$NM_ESTADO = ifelse(homicidios_base$NM_ESTADO=="ESPÍRITO SANTO","ESPIRITO SANTO",homicidios_base$NM_ESTADO)

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

mapa_base <- st_read("brasil/",layer = "UFEBRASIL") %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  st_simplify(dTolerance = 0.1)

mapa_base <- merge(x = mapa_base, y = base_dcl_rcl, by = "NM_ESTADO")
mapa_base$Anos <- as.numeric(mapa_base$Anos)

# mapa_base$DCL_RCL <- as.numeric(mapa_base$DCL_RCL)

# plota tudo
plot(mapa_base)

# mapa_base$TaxaHomicidio <- mapa_base$TaxaHomicidio/100


grafico_mapa <- mapa_base %>%
  # filter(Anos == 2017) %>%
  arrange(Perfil_Endividamento) %>%
  ggplot(aes(group = Anos)) +
  geom_sf(aes(fill=Perfil_Endividamento,frame = Anos))+
  # geom_sf_text_repel(aes(label = TaxaHomicidio))+
  geom_sf_text(aes(label = round(DCL_RCL,digits = 1)),fontface = "bold")+
  # geom_text_repel(aes(label = scales::percent(TaxaHomicidio,suffix = "%"),))+
  ggtitle("Dívida consolidada líquda (DCL) em relação à receita corrente líquida (RCL) por UF - Em %")+
  labs(caption = "Fonte: Secretaria do Tesouro Nacional (STN). Elaboração: Pedro Henrique Oliveira.\n
       Organização dos dados: IFI.",
       subtitle = "Ano: {closest_state}")+
  theme_minimal()+
  scale_fill_manual(values = c("#0c648c","#85AEC3","#D2988F","#d73027"))
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
  transition_states(states = Anos,transition_length = 1,state_length = 0,wrap = T)
# animate(grafico_mapa, fps = 3,duration = 5)
grafico_mapa_animado <- animate(grafico_mapa,fps = 1,duration = 21,height = 600,width = 800,start_pause = 1,end_pause = 1)

anim_save(filename = "dcl_rcl.gif",animation = grafico_mapa_animado)
