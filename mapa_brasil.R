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


# plotar gráficos do brasil
homicidios_base <- read.xlsx("homicidios.xlsx",sheetIndex = 2)
homicidios_base <- homicidios_base %>%
  gather(key = Estados,value = TaxaHomicidio,-Anos) %>%
  mutate(Estados = toupper(Estados)) %>%
  mutate(Estados = gsub(pattern = "\\.",replacement = " ",x = Estados)) %>%
  mutate(Estados = gsub("Ă","Ã",Estados))

names(homicidios_base) <- c("Anos","NM_ESTADO","TaxaHomicidio")

# Espírito santo dá erro
homicidios_base$NM_ESTADO = ifelse(homicidios_base$NM_ESTADO=="ESPÍRITO SANTO","ESPIRITO SANTO",homicidios_base$NM_ESTADO)



mapa_base <- st_read("brasil/",layer = "UFEBRASIL") %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  st_simplify(dTolerance = 0.1)

NM_SIGLAS <- c("DF","DF","MT","MS",
               "AL","BA","CE","MA","PB","PE","PI","RN","SE",
               "AC","AP","AM","PA","RO","RR","TO",
               "ES","MG","SP","RJ",
               "PR","RS","SC")

#NM_SIGLAS <- sort(NM_SIGLAS)

mapa_base <- cbind.data.frame(NM_SIGLAS,arrange(df = mapa_base,NM_REGIAO,NM_ESTADO))

save(mapa_base,file = "mapa_base")


#mapa_base <- merge(x = mapa_base, y = homicidios_base, by = "NM_ESTADO")



# plota tudo
plot(mapa_base)

# mapa_base$TaxaHomicidio <- mapa_base$TaxaHomicidio/100


grafico_mapa <- mapa_base %>%
  # filter(Anos == 2017) %>%
  ggplot(aes(group = Anos)) +
  geom_sf(aes(fill=TaxaHomicidio,frame = Anos))+
  # geom_sf_text_repel(aes(label = TaxaHomicidio))+
  geom_sf_text(aes(label = round(TaxaHomicidio,digits = 1)),fontface = "bold")+
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
