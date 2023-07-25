# Importacion de datos y librerias comunes ####
library(tidyverse)
library(gganimate)
library(tidyverse)
library(ggtext)
library(showtext)
showtext_auto()
font_add_google(name="Noto Sans", family = "noto")
theme_set(theme_bw(base_family = "noto"))

# df <- read_delim(url("https://datos.yvera.gob.ar/dataset/b5819e9b-5edf-4aad-bd39-a81158a2b3f3/resource/8c663c32-fee2-4a57-a918-7ab0f3819624/download/evyth_microdatos.txt"))
dict <- read_delim(url("https://datos.yvera.gob.ar/dataset/b5819e9b-5edf-4aad-bd39-a81158a2b3f3/resource/20e2c018-a2ee-4d97-9c67-a4303f669255/download/evyth_diccionario_registro.txt"))

# write_delim(df, "data-completa.txt", delim = "\t")
df<-read_delim("data-completa.txt")

# Transformaciones #### 
data <- df %>% 
  select(id_viajes, id_hogar, miembro, anio, trimestre, aglomerado_origen, provincia_destino, localidad_destino, pondera) %>% 
  filter((aglomerado_origen == 6 | aglomerado_origen == 14))

colnames(dict)[2] <- "provincia_destino"
data <- left_join(data, dict[dict$variable == "provincia_destino",], by = "provincia_destino") 

data$aglomerado_origen <- as.character(data$aglomerado_origen)
data$aglomerado_origen <- case_when(
  data$aglomerado_origen == "6" ~ "Paraná",
  data$aglomerado_origen == "14" ~ "Concordia"
)

data$localidad_destino <- gsub("José María Ezeiza",
                               replacement = "Ezeiza", 
                               x = data$localidad_destino)

data$localidad_destino <- gsub("Estación Puíggari",
                               replacement = "Puíggari", 
                               x = data$localidad_destino)

data$descripcion <- if_else(data$descripcion == "Buenos Aires (Resto)", "Buenos Aires", data$descripcion)
data$descripcion <- if_else(data$descripcion == "Partidos del GBA (Pcia. Bs. As.)", "Buenos Aires", data$descripcion)

data$localidad_destino <- if_else(data$localidad_destino == "Virgen de los tres Cerritos",
                                  "Tres Cerritos",
                                  data$localidad_destino)

data$localidad_destino <- if_else(data$localidad_destino == "Villa San Marcial (Est. Gobernador Urquiza)",
                                  "San Marcial",
                                  data$localidad_destino)

data$localidad_destino <- if_else(data$localidad_destino == "Villa Libertador San Martín",
                                  "Libertador San Martín",
                                  data$localidad_destino)

data$localidad_destino <- if_else(data$localidad_destino == "Villa José León Suárez",
                                  "José León Suárez",
                                  data$localidad_destino)

data$localidad_destino <- if_else(data$localidad_destino == "Valle de Punilla",
                                  "Punilla",
                                  data$localidad_destino)

data$localidad_destino <- if_else(data$localidad_destino == "Valle de Paravachasca",
                                  "Calamuchita",
                                  data$localidad_destino)

data$localidad_destino <- if_else(data$localidad_destino == "Santa Rosa de Río Primero (Est. Villa Santa Rosa)",
                                  "Santa Rosa de Río Primero",
                                  data$localidad_destino)

data$localidad_destino <- if_else(data$localidad_destino == "San Salvador de Jujuy (Est. Jujuy)",
                                  "San Salvador de Jujuy",
                                  data$localidad_destino)

data$localidad_destino <- if_else(data$localidad_destino == "San Miguel de Tucumán (Est. Tucumán)",
                                  "San Miguel de Tucumán",
                                  data$localidad_destino)

data$localidad_destino <- if_else(data$localidad_destino == "Saltos de Moconá",
                                  "El Soberbio",
                                  data$localidad_destino)

data$localidad_destino <- if_else(data$localidad_destino == "Puerto Las Cuevas",
                                  "Las Cuevas",
                                  data$localidad_destino)

data$localidad_destino <- if_else(data$localidad_destino == "Pueblo Liebig's",
                                  "Liebig",
                                  data$localidad_destino)

data$localidad_destino <- if_else(data$localidad_destino == "Pque. Nac. Talampaya",
                                  "La Rioja",
                                  data$localidad_destino)

data$localidad_destino <- if_else(data$localidad_destino == "Parque San Martín",
                                  "La Picada",
                                  data$localidad_destino)

data$localidad_destino <- if_else(data$localidad_destino == "Ituzaingó Centro",
                                  "Ituzaingo",
                                  data$localidad_destino)

data$localidad_destino <- if_else(data$localidad_destino == "Don Torcuato Este",
                                  "Don Torcuato",
                                  data$localidad_destino)

data$localidad_destino <- if_else(data$localidad_destino == "Ciudad del Libertad General San Martín",
                                  "Libertador San Martín",
                                  data$localidad_destino)

data$localidad_destino <- if_else(data$localidad_destino == "Capilla del Señor (Est. Capilla)",
                                  "Capilla del Señor",
                                  data$localidad_destino)

data$localidad_destino <- if_else(data$localidad_destino == "Calabacilla",
                                  "Puerto Yeruá",
                                  data$localidad_destino)

data$localidad_destino <- if_else(data$localidad_destino == "Arroyo Aguiar",
                                  "Laguna Paiva",
                                  data$localidad_destino)

# Resumen ####
resumen <- data %>% 
  select(anio, trimestre, id_viajes, id_hogar, aglomerado_origen, provincia_destino = descripcion, localidad_destino, pondera) %>% 
  filter(!(localidad_destino %in% c("999", "998", "Termas", "Río")))

resumen <- resumen %>% 
  select(-provincia_destino) %>% 
  group_by(anio, trimestre, id_viajes, id_hogar, aglomerado_origen, localidad_destino) %>% 
  summarise(viajantes = sum(pondera))

resumen <- resumen %>% 
  group_by(anio, trimestre, aglomerado_origen, localidad_destino) %>% 
  summarise(viajantes = sum(viajantes)) 

write_delim(resumen, "resumen-data.txt", delim = "\t")

# Geolocalización de ciudades ####
library(tidygeocoder)

ciudades <- data %>% 
  filter(!(localidad_destino %in% c("999", "998", "Termas", "Río"))) %>% 
  select(localidad_destino, provincia_destino = descripcion) %>% 
  unique() %>% 
  mutate(pais = "Argentina")

emisores <- tribble(
  ~localidad_destino, ~provincia_destino, ~pais,
  "Paraná", "Entre Ríos", "Argentina",
  "Concordia", "Entre Ríos", "Argentina")

ciudades <- bind_rows(ciudades, emisores) %>% 
  data.frame()

puntos <- ciudades %>%
  geocode(state = provincia_destino, city = localidad_destino, country = pais, method = "osm")

write_delim(puntos, "gobiernos-latlong.txt", delim = "\t")

localizacion <- puntos %>% 
  select(localidad_destino, long, lat)

puntos <- left_join(resumen, puntos, by = "localidad_destino", relationship = "many-to-many")

puntos <- puntos %>% 
  select(anio = anio,
         trimestre = trimestre, 
         aglomerado_origen = aglomerado_origen,
         localidad_destino,
         viajantes,
         long,
         lat) %>% 
  filter(!(localidad_destino %in% c("Guaymallén", "Capilla del Señor (Est. Capilla)", "Santa Rosa de Río Primero")))
  
geocode <- sf::st_as_sf(puntos, coords = c("long", "lat"), crs = 4326)

write_delim(geocode, "geocode.txt", delim = "\t")

# Completar los nulos
puntos_null <- puntos %>% 
  filter(is.na(long))
# Es 0 por lo que no es necesario tratar nulos

# Tratamiento de los mapas: diseño, carga de datos y visualizacion. ####

## Mapa Entre Rios ####
shape <- rgdal::readOGR(dsn = "c:/Users/sikno/Documents/github/clasificacion-sectorial/mapa/pxdptodatosok.shp", verbose = FALSE)
shapeData <- sp::spTransform(shape, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
shapeData <- sf::st_as_sf(shapeData, wkt = 'polygons', crs = st_crs(4326)) # make sf
shapeData <- rmapshaper::ms_simplify(shapeData, keep = 0.5, keep_shapes = TRUE) %>%
  filter(provincia == "Entre Ríos")

puntos <- read_delim("gobiernos-latlong.txt") %>%
  filter(provincia_destino == "Entre Ríos") %>% 
  select(localidad_destino, lat, long) 
resumen <-read_delim("resumen-data.txt")

puntos <- left_join(resumen, puntos, by = "localidad_destino", relationship = "many-to-many") %>% 
  filter(!is.na(lat))

puntos <- puntos %>% 
  select(anio = anio,
         trimestre = trimestre,
         aglomerado_origen = aglomerado_origen,
         localidad_destino,
         viajantes = viajantes,
         long,
         lat) %>% 
  filter(!is.na(lat))

puntos$fecha <- paste0("Q", puntos$trimestre, "/", substr(puntos$anio, 3,4))
puntos$fecha <- lubridate::quarter(zoo::as.yearqtr(puntos$fecha, format = "Q%q/%y"), with_year = TRUE)

### Diseño ####
eerr_map <- ggplot(shapeData) +
  geom_sf(aes(geometry = geometry), fill = "#3C4375", col = "#090D2A") +
  geom_point(data = puntos, aes(x = long, 
                                y = lat, 
                                group = anio,
                                colour = aglomerado_origen), alpha = 100) +
  geom_point(data = puntos, 
             aes(x = long, y = lat, alpha = viajantes, size = viajantes, colour = aglomerado_origen), 
             show.legend = FALSE) + 
  scale_size_continuous(range = c(1, 30)) +
  coord_sf(xlim = c(-61, -57.3), ylim = c(-30.03,-34.27)) +
  theme_bw(base_family = "noto") +
  theme(legend.position = c(1, 0),
        legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.justification = c(1,0),
        legend.text = element_text(colour = "white", face = "bold"),
        legend.title = element_text(colour = "white", face = "bold"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = "#090D2A", color = "black"),
        plot.background = element_rect(fill = "#090D2A", color = "#090D2A"),
        plot.title = element_text(colour = "white", face = "bold"),
        plot.subtitle = element_text(colour = "white"),
        plot.caption = element_text(colour = "white")) +
  guides(color = guide_legend(title.position = "top", 
                              title.hjust = .75,
                              label.position = "left")) +
  transition_time(as.integer(puntos$anio)) +
  labs(caption = "Fuente: elaboración propia en base a EVYTH",
       colour = "Origen") + 
  ease_aes("quadratic-out") +
  enter_fade() +
  exit_fade() 

animacion_eerr <- animate(eerr_map,
                          renderer = gifski_renderer(),
                          duration = 10,
                          fps = 24,
                          height = 772.8,
                          width = 515.2)

## Mapa Argentina ####
shapearg <- rgdal::readOGR(dsn = "shp-limites/provincia.shp", verbose = FALSE)
shapeDataarg <- sp::spTransform(shapearg, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
shapeDataarg <- sf::st_as_sf(shapeDataarg, wkt = 'polygons', crs = st_crs(4326)) # make sf
shapeDataarg <- rmapshaper::ms_simplify(shapeDataarg, keep = 0.5, keep_shapes = TRUE) 

puntosar <- read_delim("gobiernos-latlong.txt") %>%
  select(localidad_destino, lat, long) 
resumenar <-read_delim("resumen-data.txt")
puntosar <- left_join(resumenar, puntosar, by = "localidad_destino", relationship = "many-to-many")

puntosar <- puntosar %>% 
  select(anio = anio,
         trimestre = trimestre,
         aglomerado_origen = aglomerado_origen,
         localidad_destino,
         viajantes = viajantes,
         long,
         lat) %>% 
  filter(!is.na(lat))

puntosar$fecha <- paste0("Q", puntosar$trimestre, "/", substr(puntosar$anio, 3,4))
puntosar$fecha <- lubridate::quarter(zoo::as.yearqtr(puntosar$fecha, format = "Q%q/%y"), with_year = TRUE)

### Diseño ####
argmap <- ggplot(shapeDataarg) +
  geom_sf(aes(geometry = geometry), fill = "#3C4375", col = "#090D2A") +
  coord_sf(xlim = c(-77.815, -47.195), ylim = c(-22.755,-54.755)) +
  geom_point(data = puntosar, aes(x = long, 
                                y = lat, 
                                group = anio,
                                colour = aglomerado_origen), alpha = 100) +
  geom_point(data = puntosar,
             aes(x = long, y = lat, alpha = viajantes, size = viajantes, colour = aglomerado_origen),
             show.legend = FALSE) +
  scale_size_continuous(range = c(1, 13)) +
  theme_bw(base_family = "noto") +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = "#090D2A", color = "black"),
        plot.background = element_rect(fill = "#090D2A", color = "#090D2A"),
        plot.title = element_text(size = 9, colour = "white", face = "bold"),
        plot.subtitle = element_text(size = 8, face = "bold", colour = "white")) +
  transition_time(as.integer(puntosar$anio)) +
  labs(title = "Destino de Turistas y Excursionistas de Entre Ríos",
       subtitle = "por cuatrimestre. Año {frame_time}") + 
  ease_aes("quadratic-out") +
  enter_fade() +
  exit_fade() +
  shadow_wake(wake_length = 0.1)

animacion_arg <- animate(argmap,
                          renderer = gifski_renderer(),
                          duration = 10,
                          fps = 24,
                          height = 772.8,
                          width = 515.2)

# Combinación ####
library(magick)
a_mgif <- image_read(animacion_arg)

b_mgif <- image_read(animacion_eerr)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]), stack = FALSE)
new_gif <- image_background(new_gif, color = "#090D2A")
new_gif

for(i in 2:240){
  combined <- image_append(c(a_mgif[i], b_mgif[i]), stack = FALSE)
  new_gif <- c(new_gif, combined)
}

new_gif

# Almacenamiento en disco ####
anim_save(filename = "eerr.gif",
          animation = animacion_eerr)

anim_save(filename = "pais.gif",
          animation = animacion_arg)

anim_save(filename = "total.gif",
          animation = new_gif)

# Inferencia con api ####
library(evyth)
library(janitor)
library(tidyverse)

df <- descargar_microdato(anio = 2023, trimestre = 1)

data <- df %>% 
  filter(aglomerado_origen == 6 | aglomerado_origen == 14) %>% 
  filter(p006 >= 18) %>% 
  select(id_hogar, miembro, provincia_destino, tipo_visitante, noches = px07_agrup,
         aloj = px08_agrup, transp = px09, motiv = px10_1, quintil = quintil_pcf_visitante,
         pondera)


## Cant. Turistas ####
total <- sum(data$pondera)
turistas <- sum(data$pondera[data$tipo_visitante == 1])

DescTools::BinomCI(turistas, total, conf.level = .99, method = "clopper-pearson")

## Turistas fuera de pcia ####
turistas <- data %>% 
  filter(tipo_visitante == 1)

total <- sum(turistas$pondera)
tur.er <- sum(turistas$pondera[turistas$provincia_destino == 30]) 

DescTools::BinomCI(tur.er, total, conf.level = .99, method = "clopper-pearson")

## mca ####

mca <- data 

mca$tipo_visitante <- case_when(
  mca$tipo_visitante == 1 ~ "turista",
  mca$tipo_visitante == 2 ~ "excursionista"
)

mca$aloj <- case_when(
  mca$aloj == 0 ~ "aloj.no",
  mca$aloj == 1 ~ "aloj.2da.viv",
  mca$aloj == 2 ~ "aloj.viv.flia",
  mca$aloj == 3 ~ "aloj.viv.alq",
  mca$aloj == 5 ~ "aloj.hotel",
  mca$aloj == 6 ~ "aloj.hotel"
)

mca$noches <- case_when(
  mca$noches == 1 ~ "1 a 3 noches",
  mca$noches == 2 ~ "4 a 7 noches",
  mca$noches == 3 ~ "8 a 14 noches",
  mca$noches == 4 ~ "+15 noches",
  mca$noches == 5 ~ "+15 noches"
)
mca$noches <- if_else(is.na(mca$noches), "0 noches", mca$noches )

mca$transp <- case_when(
  mca$transp == 1 ~ "automovil",
  mca$transp == 2 ~ "omnibus",
  mca$transp == 4 ~ "avion",
  mca$transp == 6 ~ "taxi o remis"
)

mca$motiv <- case_when(
  mca$motiv == 1 ~ "recreacion",
  mca$motiv == 2 ~ "visita",
  mca$motiv == 3 ~ "trabajo",
  mca$motiv == 5 ~ "salud",
  mca$motiv == 8 ~ "otros",
)

mca$quintil <- case_when(
  mca$quintil == 1 ~ "Q1-Q2",
  mca$quintil == 2 ~ "Q1-Q2",
  mca$quintil == 3 ~ "Q3-Q4",
  mca$quintil == 4 ~ "Q3-Q4",
  mca$quintil == 5 ~ "Q5",
)

mca <- mca %>% 
  select(quintil, motiv, tipo_visitante, aloj, noches, transp, pondera)

mca %>%
  filter(tipo_visitante == "turista") %>%
  filter(transp != "avion") %>% 
  pivot_longer(-pondera, names_to = "nombre", values_to = "valor") %>% 
  ggplot() +
  geom_bar(aes(x = valor)) +
  facet_wrap(~nombre, scales = "free") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90))

# Se hace solo sobre turistas
mca <- mca %>% 
  filter(tipo_visitante == "turista") %>% 
  filter(transp != "avion") %>% 
  select(-tipo_visitante)

mca <- hutils::weight2rows(mca, weight.var = "pondera") %>% 
  data.frame() %>% 
  select(-pondera)

mca.analisis <- FactoMineR::MCA(mca)

# Noches y alojamiento tienen una fuerte asociacion
# Quintil y tipo de transporte tambien

summary(mca.analisis)

factoextra::fviz_mca_var(mca.analisis, 
                         axes = c(1,2), 
                         geom = c("point", "text"), 
                         repel = TRUE)

mca <- mca %>% 
  select(quintil, motiv)

mca.analisis <- FactoMineR::MCA(mca)
factoextra::fviz_mca_var(mca.analisis, 
                         axes = c(1,2), 
                         geom = c("point", "text"), 
                         repel = TRUE)

## eligieron ee.rr ####

df <- df %>% 
  filter(tipo_visitante == 1) %>% 
  filter(px10_1 == 1 | px10_1 == 2) %>%  
  select(id_hogar, miembro, provincia_destino, tipo_visitante, noches = px07_agrup,
         aloj = px08_agrup, transp = px09, motiv = px10_1, quintil = quintil_pcf_visitante,
         pondera)

total <- sum(df$pondera)
eerr <- sum(df$pondera[df$provincia_destino == 30])

DescTools::BinomCI(eerr, total, conf.level = .99, method = "clopper-pearson")
















