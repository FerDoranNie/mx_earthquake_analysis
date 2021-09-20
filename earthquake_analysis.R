####
# Creado por/Author by Fernando Dorantes Nieto <(°)
#                                               ( >)"
#                                                /|
####

library(magrittr)
library(data.table)
library(sf)
library(sp)
library(tmap)
library(ggplot2)
library(ggpubr)
library(lattice)


# Global Objects ----------------------------------------------------------

colors <- c("#7fc97f", "#beaed4", "#fdc086", "#386cb0",
"#f0027f", "#bf5b17", "#666666")
### Colors from https://colorbrewer2.org


# Functions ---------------------------------------------------------------

decade <- function(date){
  year <- data.table::year(date)
  y <-year - year %% 10
  return(y)
}


# Get Data ----------------------------------------------------------------
data <- fread("Downloads/SSNMX_catalogo_19000101_20210917.csv",
              header = T, skip = 4, sep=",", fill=T)
mex_map <- st_read("local/geodatos/political_maps/Estados/dest_2015gw.shp")
nombres_estados <- fread("local/nombres_estados.csv", encoding = "UTF-8",
                         header = T)

skip_last <- grep(pattern = "Fecha y hora local",
                  as.character(unlist(data[,1])))

data <- data[-skip_last:-dim(data)[1],]
names(data)<- tolower(names(data))
names(data)<- gsub("[[:space:]]", ".", names(data))

# Data Manipulation -------------------------------------------------------
data <- data %>% 
  .[, estado := gsub(".*,\\s*", "\\1", referencia.de.localizacion)] %>%  
  .[, estado := gsub("[[:space:]]","", estado)] %>% 
  .[, estado := fifelse(estado=="N", "NL", estado)] %>% 
  .[, fecha  := as.Date(fecha)] %>% 
  .[, magnitud := suppressWarnings( as.numeric(magnitud))] %>% 
  .[, magnitud.rangos := fcase(
    magnitud>=0 & magnitud<= 2 , "0-2",
    magnitud>=2 & magnitud<= 4 , "2-4",
    magnitud>=4 & magnitud<= 6 , "4-6",
    magnitud>=6 & magnitud<= 8 , "6-8",
    magnitud>=8 & magnitud<= 10 , "8-10",
    magnitud>10, "10 +",
    is.na(magnitud), "Magnitud no medida"
  )] %>% 
  .[, magnitud.rangos := factor(magnitud.rangos, 
                                levels = c("0-2", "2-4", "4-6", "6-8", "8-10",
                                           "10+", "Magnitud no medida"))] %>% 
  .[, decada := decade(fecha)] %>% 
  .[, fecha.mes := as.Date(cut(fecha, "month"))] %>% 
  .[, fecha.semana := as.Date(cut(fecha, "week"))] %>% 
  .[, mes := data.table::month(fecha)] %>% 
  .[, mes.nombre := format(fecha, "%B")] %>% 
  .[, anio := data.table::year(fecha)] %>% 
  .[fecha >= "1980-01-01"]

data <- merge(data, nombres_estados, by.x="estado", by.y="id.estado")


# Charts ------------------------------------------------------------------
data %>% 
  .[, .(conteo_sismos = .N), by=fecha] %>% 
  ggplot(aes(x= fecha, y= conteo_sismos)) +
  geom_line(color="steelblue") +
  theme_bw()+
  xlab("Fecha, días") +
  ylab("# de Sismos") +
  ggtitle("Número de sismos (Desde 1980) por día reportados por el SSN") +
  labs(caption = "El aumento de número de eventos  sísmicos reportados (localizaciones) en los últimos años responde principalmente a la instalación de nuevas estaciones sismológicas.
       No representa a un incremento de la sismicidad")


time_series2 <- data %>% 
  .[, .(conteo_sismos = .N), by=list(fecha, magnitud.rangos)] %>% 
  na.omit %>% 
  ggplot(aes(x= fecha, y= conteo_sismos, color=magnitud.rangos)) +
  geom_line(size=1)+
  scale_color_manual(values= colors, name="Rango Magnitud Scala Richter") +
  theme_bw()+
  theme(legend.position="top")+
  ggtitle("Número de sismos (Desde 1980) por día reportados por el SSN")+
  xlab("Fecha, días") +
  ylab("# de Sismos") +
  labs(caption = "El aumento de número de eventos  sísmicos reportados (localizaciones) en los últimos años responde principalmente a la instalación de nuevas estaciones sismológicas.
       No representa a un incremento de la sismicidad")

time_series3 <- data %>% 
  .[, .(conteo_sismos = .N), by=list(fecha, magnitud.rangos)] %>% 
  na.omit %>% 
  ggplot(aes(x= fecha, y= conteo_sismos, color=magnitud.rangos)) +
  geom_line(size=1)+
  scale_color_manual(values= colors, name="Rango Magnitud Scala Richter") +
  theme_bw()+
  scale_x_date(breaks = "3 year")+
  theme(legend.position="top", 
        axis.text.x = element_text(angle=90))+
  ggtitle("Número de sismos (Desde 1980) por día reportados por el SSN")+
  xlab("Fecha, días") +
  ylab("# de Sismos") +
  facet_wrap(~magnitud.rangos, scales = "free_y")+
  labs(caption = "El aumento de número de eventos  sísmicos reportados (localizaciones) en los últimos años responde principalmente a la instalación de nuevas estaciones sismológicas.
       No representa a un incremento de la sismicidad")

ggarrange(time_series2, time_series3,
          nrow = 2)


X11()
data %>% 
  .[magnitud>6] %>% 
  .[, .(conteo_sismos = .N), by=list(fecha, magnitud.rangos)] %>%
  ggplot(aes(x= (fecha), y= conteo_sismos)) +
  geom_segment( aes(x=(fecha),
                    xend=fecha, y=0, yend=conteo_sismos, 
                    color=magnitud.rangos)) +
  geom_point(size=2, alpha=0.8, aes(color= magnitud.rangos))+
  # geom_line(stat="identity")+
  scale_color_manual(values= c("steelblue", "darkred"),
                     name="Rango Magnitud Scala Richter") +
  theme_classic()+
  scale_x_date(breaks = "6 months")+
  scale_y_continuous(breaks = seq(0,2,1))+
  theme(legend.position="top",
        axis.text.x = element_text(angle=90))+
  ggtitle("Número de sismos de escala mayor o igual a 6 (Desde 1980) por día reportados por el SSN")+
  xlab("Fecha, días") +
  ylab("# de Sismos") #+
  # facet_wrap(~magnitud.rangos, scales = "free")



data %>% 
  .[, .(conteo_sismos = .N), by=list(mes, mes.nombre)] %>% 
  .[order(mes)] %>% 
  ggplot(aes(x= reorder(mes.nombre, mes), y= conteo_sismos)) +
  geom_bar(stat = "identity", fill="steelblue")+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  geom_label(aes(reorder(mes.nombre, mes), y= conteo_sismos,
                 label=conteo_sismos) ) +
  xlab("Mes") +
  ylab("Conteo Sismos")+
  ggtitle("Número de sismos (Desde 1980) por mes reportados por el SSN")

data %>% 
  .[, .(conteo_sismos = .N), by=list(mes, mes.nombre)] %>% 
  .[order(mes)] %>% 
  ggplot(aes(x= reorder(mes.nombre, -conteo_sismos), y= conteo_sismos)) +
  geom_bar(stat = "identity", fill="steelblue")+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  geom_label(aes(reorder(mes.nombre, mes), y= conteo_sismos,
                 label=conteo_sismos) ) +
  xlab("Mes") +
  ylab("Conteo Sismos")+
  ggtitle("Número de sismos (Desde 1980) por mes reportados por el SSN",
          subtitle = "Ordenados de menor a mayor")


barchart_rangos1 <- data %>% 
  .[, .(conteo_sismos = .N), by=list(mes.nombre, mes, magnitud.rangos)] %>% 
  ggplot(aes(x= reorder(mes.nombre, mes),
             y= conteo_sismos, fill= magnitud.rangos)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90),
        legend.position="top") +
  xlab("Mes") +
  ylab("Conteo Sismos")+
  ggtitle("Número de sismos (Desde 1980) por mes reportados por 
          el SSN",
          subtitle = "Agrupados por magnitud escala Richter")

  

barchart_rangos2 <- data %>% 
  .[, .(conteo_sismos = .N), by=list(mes.nombre, mes, magnitud.rangos)] %>% 
  ggplot(aes(x= reorder(mes.nombre, mes),
             y= conteo_sismos, fill= magnitud.rangos)) +
  geom_bar(stat = "identity", position="fill") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,0.1))+
  theme(axis.text.x = element_text(angle=90),
        legend.position="top") +
  xlab("Mes") +
  ylab("Porcentaje Sismos")+
  ggtitle("Número de Sismos (Desde 1980) vistos en porcentaje",
          subtitle = "Agrupados por magnitud escala Richter")


ggarrange(barchart_rangos1, barchart_rangos2, ncol=2)

data %>% 
  .[magnitud>6] %>% 
  .[, .(conteo_sismos = .N), by=list(mes.nombre, mes, magnitud.rangos)] %>% 
  ggplot(aes(x= reorder(mes.nombre, mes),
             y= conteo_sismos, fill= magnitud.rangos)) +
  geom_bar(stat = "identity")+
  theme_bw() +
  scale_y_continuous(breaks = seq(0,100,1))+
  theme(axis.text.x = element_text(angle=90),
        legend.position="top")+
  scale_fill_manual(values= c("steelblue", "darkred"),
                     name="Rango Magnitud Escala Richter")+  
  xlab("Mes") +
  ylab("Conteo Sismos")+
  geom_label(aes(reorder(mes.nombre, mes), y= conteo_sismos,
                 label=conteo_sismos) )+
  ggtitle("Número de Sismos (Desde 1980) con escala Richter mayor a 6",
          subtitle = "Agrupados por magnitud")

X11()
data %>% 
  .[, .(conteo_sismos = .N), by=list(nombre.estado)] %>% 
  merge(., mex_map, by.x="nombre.estado", by.y="NOM_ENT") %>% 
  st_as_sf() %>% 
  ggplot()+
  geom_sf(aes(fill = conteo_sismos))

tmap_style("classic")
mapa_estados <- data %>% 
  .[, .(conteo_sismos = .N), by=list(nombre.estado)] %>% 
  merge(., mex_map, by.x="nombre.estado", by.y="NOM_ENT") %>% 
  st_as_sf() %>% 
  tm_shape() +
  tm_polygons("conteo_sismos", #palette = "RdYlBu",
              title = "Rango número de sismos")+
  tm_layout("Número de sismos 
            por estado (Desde 1980)", title.position = c('right', 'top'))

X11()
mapa_estados

pareto_data <- data %>% 
  .[, .(conteo_sismos = .N), by=list(nombre.estado)]  %>% 
  .[, total := sum(conteo_sismos)] %>% 
  .[order(conteo_sismos, decreasing = T)] %>% 
  .[, suma_acumulada := cumsum(conteo_sismos)] %>% 
  .[, porcentaje := conteo_sismos/total] %>% 
  .[, porcentaje_acumulado := suma_acumulada/total]

porcentaje_lista <- 0:100

barras_estados <- ggplot(data= data.frame(pareto_data), 
       aes(x=nombre.estado)) +
  geom_bar(aes(x=reorder(nombre.estado, -conteo_sismos), y=conteo_sismos), 
           fill='steelblue', stat="identity") + 
  scale_y_continuous(limits = c(0, (max(pareto_data$conteo_sismos))+15000 ))+
  theme_bw()+
  # theme(axis.text.x = element_text(angle=45, hjust=1))+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())+
  # xlab("Mes") +
  ylab("Conteo Sismos")+
  geom_text(aes(reorder(nombre.estado, -conteo_sismos), y= conteo_sismos,
                 label=conteo_sismos), vjust=-1, angle=45, hjust=0)+
  ggtitle("Número de Sismos (Desde 1980)",
          subtitle = "Agrupados por estado")

porcentaje_estados <- ggplot(data= data.frame(pareto_data)) +
  geom_bar(aes(x=reorder(nombre.estado, -conteo_sismos), y=porcentaje), 
           fill='steelblue', stat="identity") + 
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  scale_y_continuous(labels = scales::percent, 
                     breaks = seq(0,1,0.1), 
                     limits=c(0,0.7))+
  xlab("Mes") +
  ylab("Conteo Sismos")+
  geom_text(aes(x= reorder(nombre.estado, -conteo_sismos),
                y = porcentaje,
                label=paste0(round(porcentaje*100, 3),"%"),
                vjust=0, angle=90, hjust=0))+
  ggtitle("Porcentaje de sismos por estado (Desde 1980)")


ggarrange(barras_estados, porcentaje_estados,
           nrow = 2)


top_terremotos <- data %>%  
  .[magnitud>=7.5]


X11()
ggplot(data = mex_map) +
  geom_sf()+
  geom_point(data= top_terremotos, 
             aes(x= longitud, y= latitud,
                 size= magnitud), color="steelblue")+
  theme_bw() +
  scale_color_manual( name="Magnitud escala de Richter")+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position="top")+
  ggrepel::geom_text_repel(data= top_terremotos, 
                           aes(x= longitud, y= latitud,
                               label= magnitud), color="darkred")+
  ggtitle("Coordenadas sismos con Magnitud superior o igual a 7.5
          en escala de Richter (Desde 1980)")


# geom_point(aes(x=nombre.estado, y=porcentaje), color = rgb(0, 1, 0), pch=16, size=1)+
  # scale_y_continuous(
  #   name = "Conteo Sismos",
  #   # sec.axis = sec_axis(~(.*0.000001), name="Porcentaje acumulado")
  #   sec.axis = sec_axis(porcentaje_lista, name="Porcentaje acumulado")
  # )
  #geom_path(aes(y=pareto_data$porcentaje, group=1), colour="slateblue1", lty=3, size=0.9) 

  

