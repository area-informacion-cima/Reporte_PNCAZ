
##########################################
# generar tablas y gráficos descriptivos #
# para el reporte trimestral             #
##########################################

# especificar la ruta
getwd()
# abrir los datos: " smart_data_t1.RDA "
#setwd("C:/Users/modicio/Documents/Proyecto_RPNCAZ/Reporte_PNCAZ")
#dir()

# instalar librerías
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(kableExtra)
library(modelsummary)

#### esfuerzo de patrullaje #####
class(datos)

min(datos$inicio_patrullaje)
max(datos$final_patrullaje)

unique(datos$posicion_patrullaje)
unique(datos$tipo_patrullaje)
unique(datos$pvc_pncaz)

###
datos |>
  group_by(id_patrullaje, pvc_pncaz, tipo_patrullaje) |>
  summarize(
    N_patrullajes = n()
  )


test_4 <- datos |>
  group_by(id_patrullaje, pvc_pncaz, tipo_patrullaje) |>
  summarise(N = n()) |>
  arrange(desc(N))

datasummary_skim(test_4, type = "categorical", 
                 output = "markdown") # tabla 1

datasummary_skim(test_4, type = "categorical")

datasummary(pvc_pncaz * tipo_patrullaje + 1 ~ N + Percent(), data = test_4) # tabla 2

datasummary(tipo_patrullaje * pvc_pncaz + 1 ~ N + Percent(), data = test_4) # tabla 3

datasummary_crosstab(pvc_pncaz ~ tipo_patrullaje, data = test_4) # table 4

#### gráfico patrullaje #####
# plot 1
  ggplot(
    data = test_4,
    mapping = aes(y=tipo_patrullaje,
                  fill=tipo_patrullaje)) +
  geom_bar()

### LOAD resumen datos de Patrullaje ####
### resumen por tipo de patrullaje
setwd("U:/Reporte_PNCAZ/datos")
dir()
resumen <- read.csv("resumen_puestos_patrullaje_T1.csv", header = T, sep = ";")
resumen |>
  group_by(PVC)

knitr::kable(resumen, caption = "Tabla de ejemplo")

knitr::kable(resumen, caption = "Tabla de ejemplo")

### resumen por ubicación
ubicacion <- read.csv("resumen_patrullaje_T1.csv", header = T, sep = ";")
knitr::kable(ubicacion, caption = "Patrullajes en la ZA y dentro del PNCAZ")

#### Gráfico Apoyos Comunales ####
setwd("U:/Reporte_PNCAZ/datos")

apoyos<-read.csv(file = "Resumen_Apoyos_Comunales.csv", header = T,
                 sep = ";", stringsAsFactors = F)
apoyos |>
  group_by(Sede, sexo, Monto_pagado_.S...)|>
  ggplot(aes(x=Sede, y=Monto_pagado_.S..., 
             fill=sexo)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  # Agregar etiquetas numéricas dentro de las barras
  labs (x="Sedes",
        y="Monto Pagado (S/.)",
        fill="Sexo") + 
  theme_minimal()


#### Opiniones técnicas ####
# Crear la tabla con HTML para colorear y dar formato a las celdas específicas
#### tabla html ####
tabla_html <- data.frame(
  Solicitudes_totales = c(
    "<span style='font-weight:bold; font-size:larger;'>Solicitudes totales</span>", 
    "<span style='font-weight:bold; font-size:larger;'>Opiniones técnicas</span>", 
    "Concesiones, Plan de manejo forestal, DEMA", 
    "Instalación de Grifos", "Actividad acuícola (piscigranjas)", 
    "Red de distribución de telecomunicaciones (telefonía, internet, radio, otros)", 
    "Petitorio minero", "Proyecto de cultivo y/o asistencia técnica (plátano, cacao, café, bambú, arroz, otros)", 
    "Muro de Protección, defensa ribereña y/o recuperación de faja marginal de rio", 
    "Estación de telecomunicaciones (telefonía, internet, radio, otros)", 
    "Uso de agua para actividad agrícola y/o riego", 
    "Construcción de infraestructura (instituciones, hospital, embarcadero, otros)", 
    "Construcción y/o mantenimiento de caminos (trocha, puente)", 
    "Instalación de servicios y/o sistemas para distribución eléctrica (paneles, cables, otros)", 
    "Construcción de infraestructura ecoturística (albergues)", 
    "Establecimiento de bosque local, concesión para conservación, reforestación y/o restauración", 
    "Extracción de aceite de palma", 
    "Instalación o modificación de agua y desagüe", 
    "Mejoramiento de capacidades para crianza de animales menores", 
    "Construcción y/o mejoramiento de drenaje pluvial", 
    "<span style='font-weight:bold; font-size:larger;'>Solicitudes generales</span>", 
    "Consultas generales", "Alertas tempranas (mensuales) focos de calor - SERFOR", 
    "Caso Puerto Franco", "Caso Central Hidroeléctrica Alto Biavo", 
    "Cultivos ilícitos", "Incendio forestal poblado Jorge Basadre", 
    "Otros formatos (no mapa)", "Ubicación-General", "Capacitaciones", 
    "Deforestación", "Registro de SMART"
  ),
  Numero = c(
    "<span style='font-weight:bold; font-size:larger;'>N=132</span>", 
    "<span style='font-weight:bold; font-size:larger;'>N=26</span>", 
    "3", "1", "2", "0", "2", "0", "1", "3", "1", "1", "3", "3", "1", "0", "0", "4", 
    "0", "1", 
    "<span style='font-weight:bold; font-size:larger;'>N=106</span>", 
    "56", "10", "0", "0", "0", "0", "0", "2", "3", "29", "6"
  ),
  Impacto_potencial = c(
    "<span style='font-weight:bold; font-size:larger;'>Impacto potencial</span>", 
    "", 
    "<span style='color:white; background-color:#FFD700;'>Medio</span>", 
    "<span style='color:white; background-color:green;'>Bajo</span>", 
    "<span style='color:white; background-color:green;'>Bajo</span>", 
    "<span style='color:white; background-color:#FFD700;'>Medio</span>", 
    "<span style='color:white; background-color:#FFD700;'>Medio</span>", 
    "<span style='color:white; background-color:red;'>Alto</span>", 
    "<span style='color:white; background-color:#FFD700;'>Medio</span>", 
    "<span style='color:white; background-color:green;'>Bajo</span>", 
    "<span style='color:white; background-color:#FFD700;'>Medio</span>", 
    "<span style='color:white; background-color:green;'>Bajo</span>", 
    "<span style='color:white; background-color:#FFD700;'>Medio</span>", 
    "<span style='color:white; background-color:#FFD700;'>Medio</span>", 
    "<span style='color:white; background-color:green;'>Bajo</span>", 
    "<span style='color:white; background-color:green;'>Bajo</span>", 
    "<span style='color:white; background-color:#FFD700;'>Medio</span>", 
    "<span style='color:white; background-color:green;'>Bajo</span>", 
    "<span style='color:white; background-color:green;'>Bajo</span>", 
    "<span style='color:white; background-color:green;'>Bajo</span>", 
    "", 
    "<span style='color:white; background-color:green;'>Bajo</span>", 
    "<span style='color:white; background-color:red;'>Alto</span>", 
    "<span style='color:white; background-color:#FFD700;'>Medio</span>", 
    "<span style='color:white; background-color:#FFD700;'>Medio</span>", 
    "<span style='color:white; background-color:red;'>Alto</span>", 
    "<span style='color:white; background-color:red;'>Alto</span>", 
    "<span style='color:white; background-color:green;'>Bajo</span>", 
    "<span style='color:white; background-color:green;'>Bajo</span>", 
    "<span style='color:white; background-color:green;'>Bajo</span>", 
    "<span style='color:white; background-color:red;'>Alto</span>", 
    "<span style='color:white; background-color:green;'>Bajo</span>"
  )
)
# save in rda
save(tabla_html, file = "opinion_Tecnica.RDA")

#### Renderizar la tabla con kable sin encabezados####
kable(tabla_html, format = "html", escape = F, col.names = NULL) %>%
  kable_styling("striped", full_width = F)


########### Crear la tabla sin HTML #############
# Crear la tabla de datos (sin HTML)
tabla_sinhtml <- data.frame(
  Solicitudes_totales = c(
    "Solicitudes totales", 
    "Opiniones técnicas", 
    "Concesiones, Plan de manejo forestal, DEMA", 
    "Instalación de Grifos", "Actividad acuícola (piscigranjas)", 
    "Red de distribución de telecomunicaciones (telefonía, internet, radio, otros)", 
    "Petitorio minero", "Proyecto de cultivo y/o asistencia técnica (plátano, cacao, café, bambú, arroz, otros)", 
    "Muro de Protección, defensa ribereña y/o recuperación de faja marginal de rio", 
    "Estación de telecomunicaciones (telefonía, internet, radio, otros)", 
    "Uso de agua para actividad agrícola y/o riego", 
    "Construcción de infraestructura (instituciones, hospital, embarcadero, otros)", 
    "Construcción y/o mantenimiento de caminos (trocha, puente)", 
    "Instalación de servicios y/o sistemas para distribución eléctrica (paneles, cables, otros)", 
    "Construcción de infraestructura ecoturística (albergues)", 
    "Establecimiento de bosque local, concesión para conservación, reforestación y/o restauración", 
    "Extracción de aceite de palma", 
    "Instalación o modificación de agua y desagüe", 
    "Mejoramiento de capacidades para crianza de animales menores", 
    "Construcción y/o mejoramiento de drenaje pluvial", 
    "Solicitudes generales", 
    "Consultas generales", "Alertas tempranas (mensuales) focos de calor - SERFOR", 
    "Caso Puerto Franco", "Caso Central Hidroeléctrica Alto Biavo", 
    "Cultivos ilícitos", "Incendio forestal poblado Jorge Basadre", 
    "Otros formatos (no mapa)", "Ubicación-General", "Capacitaciones", 
    "Deforestación", "Registro de SMART"
  ),
  Numero = c(
    "N=132", 
    "N=26", 
    "3", "1", "2", "0", "2", "0", "1", "3", "1", "1", "3", "3", "1", "0", "0", "4", 
    "0", "1", 
    "N=106", 
    "56", "10", "0", "0", "0", "0", "0", "2", "3", "29", "6"
  ),
  Impacto_potencial = c(
    "Impacto potencial", 
    "", 
    "Medio", 
    "Bajo", 
    "Bajo", 
    "Medio", 
    "Medio", 
    "Alto", 
    "Medio", 
    "Bajo", 
    "Medio", 
    "Bajo", 
    "Medio", 
    "Medio", 
    "Bajo", 
    "Bajo", 
    "Medio", 
    "Bajo", 
    "Bajo", 
    "Bajo", 
    "", 
    "Bajo", 
    "Alto", 
    "Medio", 
    "Medio", 
    "Alto", 
    "Alto", 
    "Bajo", 
    "Bajo", 
    "Bajo", 
    "Alto", 
    "Bajo"
  )
)

# Cargar las bibliotecas necesarias
library(knitr)
library(kableExtra)
#### Renderizar la tabla con kable sin encabezados####

save(tabla_sinhtml, file="C:/Users/modicio/Documents/Proyecto_RPNCAZ/Reporte_PNCAZ/rda/Sinhtm_Opiniones.RDA")
load("C:/Users/modicio/Documents/Proyecto_RPNCAZ/Reporte_PNCAZ/rda/Sinhtm_Opiniones.RDA")
kable(tabla_sinhtml, format = "markdown", escape = F, col.names = NULL) %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(c(1,2,21), bold=TRUE, color = "black")

############################
library(readxl)
#leer el archivo excel
tabla<-read_excel("U:/Reporte_PNCAZ/datos/Resumen_investigaciones.xlsx")
glimpse(tabla)
tabla
save(tabla, file="U:/Reporte_PNCAZ/rda/resumen_investigaciones.RDA")
# Convertir a tabla para publicación
kable(tabla, format = "html", escape = F) %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(c(1,3,6,16,21), bold="T", color = "black")

############## para exportar en word ########
# primer opcion
# Instalar y cargar el paquete
# install.packages("rmarkdown")
library(rmarkdown)
# Definir el archivo de entrada y salida
getwd()
setwd("U:/Reporte_PNCAZ_2024")
input_file <- "U:/Reporte_PNCAZ_2024/index.html"
output_file <- "U:/Reporte_PNCAZ_2024/index.docx"

# Exportar el archivo HTML a Word sin estilo de referencia
rmarkdown::pandoc_convert(input_file, to = "docx", output = output_file)

############ tabla de turismo: 3.3.3.1 Mejora de infraestructura
# Crear el data frame
library(tidyverse)
library(kableExtra)
tabla <- data.frame(
  Comunidad = c("Consuelo", "Consuelo", "Consuelo", "Consuelo", "Consuelo", "Consuelo",
                "Jorge Basadre", "Maronilla", "Maronilla", "Maronilla", "Maronilla", "Maronilla"),
  Recurso = c("Cueva de Consuelo", "Cueva de Consuelo", "Cueva de Consuelo", "Cueva de Consuelo", "Cueva de Consuelo", "Cueva de Consuelo",
              "Río Azul", "Catarata Otorongo", "Catarata Otorongo", "Catarata Otorongo", "Catarata Otorongo", "Catarata Otorongo"),
  Estructuras_instaladas = c(
    "Un punto ecológico con 03 tachos de basura con techo", 
    "Un vestuario para visitantes", 
    "Una boletería", 
    "Un letrero de señalización", 
    "Un letrero orientativo", 
    "", 
    "01 punto ecológico con 03 tachos de basura con techo", 
    "En proceso de construcción:", 
    "Una boletería", 
    "Una maloca de descanso", 
    "Letrero de señalética", 
    "Punto Ecológico"
  ),
  stringsAsFactors = FALSE
)

# Rellenar con valores en blanco para las columnas Comunidad y Recurso
tabla$Comunidad[duplicated(tabla$Comunidad)] <- ""
tabla$Recurso[duplicated(tabla$Recurso)] <- ""

# Ver el data frame
print(tabla)

save(tabla, file="U:/Reporte_PNCAZ/rda/turismo_infraestructura.RDA")


# Visualizar la tabla usando kable y kableExtra
library(knitr)
library(kableExtra)
kable(tabla, format = "markdown", col.names = c("Comunidad", "Recurso", "Estructuras instaladas"), escape = FALSE) %>%
  kable_styling("striped", full_width = F)


######## Actividad 1.3.3
getwd()
convenio<-read_xlsx("U:/Reporte_PNCAZ/datos/Tabla_Act133.xlsx")

kable(convenio, format = "markdown", col.names = c("Artículos donados", "Cantidad"), escape = FALSE) %>%
  kable_styling("striped", full_width = F)

######## Resumen PCV Implementados ########
library(readxl)
library(kableExtra)
library(tidyverse)

#leer el archivo excel
implementados<-read_excel("U:/Reporte_PNCAZ/datos/Resumen_Implementados-2024.xlsx")
glimpse(implementados)
implementados$Hito[18] <- "capacitación en temas de turismo"

implementados
setwd("U:/Reporte_PNCAZ/rda")
save(implementados, file = "resumen_implementados2024.RDA")

# No repetir nombres
rm(list = ls())
setwd("U:/Reporte_PNCAZ/rda")
load("resumen_implementados2024.RDA")
implementados<-implementados %>%
  mutate(Sede = ifelse(duplicated(Sede), "", Sede),
         Comunidad = ifelse(duplicated(Comunidad), "", Comunidad))
# Crear la tabla con kable y agregar los hipervínculos en la columna "Hito"
# en markdown
implementados |>
  mutate(Hito = paste0("[", Hito, "](", Link, ")")) |>
  select(-Link) %>%  # Eliminar la columna de URLs
  kable(format = "markdown", escape = F) |>
  kable_styling("striped", full_width = F) |>
  row_spec(0, bold = TRUE, color = "black")

# en HTML
implementados %>%
  mutate(Hito = sprintf('<a href="%s" target="_blank">%s</a>', Link, Hito)) %>%
  select(-Link) %>%  # Eliminar la columna de URLs
  kable(format = "html", escape = F) %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(0, bold = TRUE, color = "black")

# otros ejemplos
implementados %>%
  mutate(Hito = paste0('<a href="', Link, '" target="_blank">', Hito, '</a>')) %>%
  select(- Link) %>%
  kable(format = "markdown", escape = F) %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(0, bold = TRUE, color = "black")

implementados %>%
  mutate(Hito = paste0("[", Hito, "](", Link, ")")) %>%
  select(-Link) %>%  # Eliminar la columna de URLs
  kable(format = "markdown", escape = F) %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(0, bold = TRUE, color = "black")

############ Fondo Semilla #######
setwd("U:/Reporte_PNCAZ_2024/datos")
fs<-read.csv(file = "resumen_fs.csv", header = T, sep=";",na.strings = "NA",
             fileEncoding = "latin1")

fs<-read_xlsx("resumen_fs.xlsx" )

fs<-glimpse(fs)
fs[is.na(fs)]<-0

## remplazar caracteres
fs$Producto
fs$Organizaciones
# Reemplazar caracteres específicos para una variable
unique(fs$Organizaciones)
# esta es una forma de hacerlo fs$Organizaciones <- gsub("Ã©", "é", fs$Organizaciones)

# Remplazar con una función
# 1. Crear una función para reemplazar caracteres específicos
replace_special_characters <- function(text) {
  replacements <- c(
    "Ã©" = "é",
    "Ã³" = "ó",
    "Ã±" = "ñ",
    "Ã­a" = "ía",
    "Ã­" = "í",
    "Ã" = "í",
    "Â" = "",
    "Ã" = ""
  )
  for (i in seq_along(replacements)) {
    text <- gsub(names(replacements)[i], replacements[i], text)
  }
  return(text)
}
# 2. Aplicar la función a la columna de interés
fs$Organizaciones <- replace_special_characters(fs$Organizaciones)
fs$Producto <- replace_special_characters(fs$Producto)
fs$Comunidad <- replace_special_characters(fs$Comunidad)
### save as rda ###
getwd()
setwd("U:/Reporte_PNCAZ_2024/rda")
save(fs, file = "fondo_semilla_T1-2024.RDA")
rm(list = ls())
# Crear un objeto para seleccionar los datos y crear la tabla

load("fondo_semilla_T1-2024.RDA")

fstable<-fs|>
  select(Sede,Año_n, Año, Comunidad,Organizaciones, Codigo_fs, N_Familias, 
         Tipo_apoyo, Producto, Periodo_meses, Primer_desembolso,
         `Trimestre_primer desembolso`, Segundo_desembolso,Año_segundo_desembolso, 
         `Trimestre_primer desembolso2`) |>
  filter(Año==2024) |>
  select(Sede,Comunidad,Organizaciones,Codigo_fs,N_Familias, Tipo_apoyo, Producto,Periodo_meses, Primer_desembolso) |>
  arrange(Sede) |>
  mutate(Sede = ifelse(duplicated(Sede), "", Sede))

kable(fstable, format = "markdown", col.names = c("Sede", "Comunidad","Organizaciones","Codigo FS","familias","Tipo de apoyo","Producto", "Duración del proyecto","Primer desembolso"), table.attr = "class='table table-bordered'") |>
    kable_styling(bootstrap_options =c("striped","hover"), full_width = F) |>
    row_spec(0, bold = TRUE, color = "black") |>
    row_spec(c(1, 2), background = "#CCCCFF")|>
    row_spec(c(3,4,5,6,7), background = "#FFCC99") |>
    row_spec(c(8,9,10,11), background = "#99FF99")

    ## Para guardar como SVG:

## Instalar y cargar los siguientes paquetes:
install.packages(c("knitr", "kableExtra", "webshot", "rsvg", "htmltools"))
library(tidyverse)
library(knitr)
library(kableExtra)
library(webshot)
library(rsvg)
library(htmltools)
library(pagedown)

# Guardar la tabla HTML en un archivo temporal
html_file <- tempfile(fileext = ".html")
save_html(fstable, file = html_file)
# Convertir HTML a PNG
png_file <- tempfile(fileext = ".png")
pagedown::chrome_print(input = html_file, output = png_file, selector = "table")



# Definir una paleta de colores: ejemplo
colors <- c("#FF9999", "#99CCFF", "#99FF99", "#FFCC99", "#CCCCFF")
# Generar la tabla con kable y aplicar estilos
# Aplicar colores a las filas específicas
for (i in 1:nrow(table1)) {
  table1_kable <- table1_kable %>%
    row_spec(i, background = colors[(i %% length(colors)) + 1])
}
########### FOTP #########
library(readxl)
setwd("C:/Users/modicio/Documents/Proyecto_RPNCAZ/Reporte_PNCAZ/datos")
fotp <-read_excel("Detalle_FOTP_al_16_04_2024.xlsx")

### Cambiar todas las vacias de la columna 
### fotp$Sub_actividad ### 
### Inicialmente todas estas actividades se encontraban reportadas en el PCV
### pero corresponden al FOTP. Así que FER las ha colocado en esta tabla
fotp<-fotp |> 
  filter(Año==2024) |>
  mutate(Sub_actividad = if_else(is.na(Sub_actividad),
                                 "5.2.2.1 FOTP (para comunidades con PCV)",
                                Sub_actividad))
### seleccionar las variables importantes para el reporte
fotp |>
  filter(Año==2024, Trimestre == 1) |>
  select(Sector, Tipo, `Comunidad (es)`, `Tipo de organización`, Organización, ID_Org,
         `Producto / Servicio`, `Familias beneficiadas`, `N° de socios(as)`,
         `Socio Mujer`, `Socio Hombre`,`N° de participantes`, `N° participantes mujeres`,
         `N° participantes hombres`, `Cantidad hito`, `Hito obtenido (estos son los hitos del trimestre)`,
         Tematica, Actores, Sub_actividad, `Nombre de la prioridad`, `Etapa (de la organización)`)
setwd("C:/Users/modicio/Documents/Proyecto_RPNCAZ/Reporte_PNCAZ/rda")
save(fotp, file = "fotp_t1_2024.RDA")
rm(list = ls())
### comunidades con pcv y fotp
getwd()
load("C:/Users/modicio/Documents/Proyecto_RPNCAZ/Reporte_PNCAZ/rda/fotp_t1_2024.RDA")

fotp_pcv <- fotp |>
  filter(Año==2024, Trimestre == 1, Sub_actividad=="5.2.2.1 FOTP (para comunidades con PCV)" ) |>
  select(Sector, Tipo, `Comunidad (es)`, `Tipo de organización`, Organización, ID_Org,
         `Producto / Servicio`, `Hito obtenido (estos son los hitos del trimestre)`,
         Tematica, `Nombre de la prioridad`, `Etapa (de la organización)`,`N° de socios(as)`,
         `Socio Mujer`, `Socio Hombre`)    
### comunidades sin pcv con fotp
fotp_sinpcv <- fotp |>
  filter(Año==2024, Trimestre == 1, Sub_actividad=="5.2.3.2 Fortalecimiento Organizacional Técnico Productivo (no cuentan con PCV)") |>
  select(Sector, Tipo, `Comunidad (es)`, `Tipo de organización`, ID_Org,
         `Producto / Servicio`, `Hito obtenido (estos son los hitos del trimestre)`,
         Tematica, `Nombre de la prioridad`, `Etapa (de la organización)`,`N° de socios(as)`,
         `Socio Mujer`, `Socio Hombre`)
#### resultados del apoyo del FOTP para comunidades con y sin pcv ####

fotp$`N° de socios(as)`<- as.numeric(fotp$`N° de socios(as)`)
tabla_resumen_pcv <- fotp |>
  group_by(Sub_actividad) |>
  filter(Año==2024) |>
  summarize(
    numero_sectores = n_distinct(Sector),
    numero_comunidades = n_distinct(`Comunidad (es)`),
    numero_organizaciones = n_distinct(Organización),
    numero_productos = n_distinct(`Producto / Servicio`),
    numero_hitos_logrados = n_distinct(`Hito obtenido (estos son los hitos del trimestre)`),
    numero_prioridades = n_distinct(`Nombre de la prioridad`)
)

test<-as.data.frame(t(tabla_resumen_pcv))
colnames(test)<-NULL
rownames(test)<-c("Resultados", "n°sectores", "n°comunidades", "n°organizaciones",
                  "n°productos", "n° hitos logrados", "n°prioridades")
kable(test, format = "markdown", escape = FALSE) |>
  row_spec(1, bold = TRUE, background = "lightgrey") %>%  # Encabezado en negrita
    kable_styling("striped", full_width = F)


### productos y servicios ###

productos<-fotp |>
  filter(Año==2024, Trimestre == 1) |>
  select(Sector, Tipo, Organización, `Producto / Servicio`, 
         Tematica, `Etapa (de la organización)`)


### convertir tabla a objeto y presentar ####
setwd("C:/Users/modicio/Documents/Proyecto_RPNCAZ/Reporte_PNCAZ/datos")
patrullaje<-read_xlsx("Tabla_Patrullajes.xlsx")
kable(patrullaje, format = "markdown", escape = FALSE, align = c('c', 'c')) |>
  row_spec(0, bold = TRUE, background = "#CCCCFF") |>  # Encabezado en negrita
  kable_styling("striped", full_width = F) 
  
#### 
setwd("C:/Users/modicio/Documents/Proyecto_RPNCAZ/Reporte_PNCAZ/datos")
dir()
n_pat<- read_xlsx("N_total_patrullaje.xlsx")
kable(n_pat, format = "markdown", escape = FALSE, align = c('c', 'c')) |>
  row_spec(0, bold = TRUE, background = "#CCCCFF") |>
  kable_styling("striped", full_width = F)


### Crear un Script para Extraer los Hipervínculos #######
library(xml2)
library(rvest)
library(dplyr)

# Leer el archivo .qmd como texto
qmd_content <- readLines("index.qmd")

# Convertir el contenido a un solo string
qmd_content <- paste(qmd_content, collapse = "\n")

# Añadir la estructura mínima de HTML para poder leerlo con xml2
qmd_content_html <- paste0("<html><body>", qmd_content, "</body></html>")

# Analizar el contenido HTML
qmd_html <- read_html(qmd_content_html)


# Extraer hipervínculos
hyperlinks <- qmd_html %>%
  html_nodes("a") %>%
  html_attr("href")

# Extraer texto de los hipervínculos
link_text <- qmd_html %>%
  html_nodes("a") %>%
  html_text()

# Crear un data frame con los hipervínculos y su texto asociado
hyperlinks_df <- data.frame(text = link_text, link = hyperlinks, stringsAsFactors = F)

# Mostrar los hipervínculos
print(hyperlinks_df)


###############
### tercera opción crear una tabla de Anexos e hipervínculos ###
### Esta funcionó ####"

# Abrir la librería 
library(stringr)

# Leer el archivo .qmd como texto
qmd_content <- readLines("index.qmd")

# Definir la expresión regular para extraer los hipervínculos
regex <- "\\[([^\\]]+)\\]\\(([^\\)]+)\\)"

# Extraer todos los hipervínculos y sus textos
matches <- str_match_all(qmd_content, regex)[[1]]

# Crear un data frame con los textos y los hipervínculos
hyperlinks_df <- data.frame(text = matches[, 2], link = matches[, 3], stringsAsFactors = FALSE)

# Mostrar los hipervínculos
print(hyperlinks_df)

####### yaml original
#---
#  title: "Reporte T1-2024"
#author: "CIMA"
#format: 
#  html:
#  toc: true
#knitr: 
#  opts_chunk:
#  warning : false
#message : false
#lang: es
#editor: visual
#bibliography: references.bib
#---

### tabla comisión de seguimiento
setwd("U:/Reporte_PNCAZ_2024/datos")
library(readxl)
library(tidyverse)
library(kableExtra)
seguimiento<-read_xlsx("Comision_Seguimiento.xlsx" )

seguimiento<-seguimiento |>
  filter(Año==2023)
# esta tabla no es ideal porque se repite, hay que colapsar los
# nombres de las columnas que se repiten
kable(seguimiento, format = "markdown", escape = FALSE, align = c('c', 'c')) |>
  row_spec(0, bold = TRUE, background = "#CCCCFF") |>
  kable_styling("striped", full_width = F)

seguimiento$Año
seg_colapsado<-seguimiento |>
  group_by(Año, `Resolución de conformación de la Comisión`,`Fecha de realización`) |>
  summarise(Miembros = paste(`Miembros de la Comisión`, 
            collapse = "\n"), .groups = "drop")
kable(seg_colapsado, format = "markdown", escape = FALSE, align = c('c', 'c')) |>
  row_spec(0, bold = TRUE, background = "#CCCCFF") |>
  kable_styling("striped", full_width = F) |>
  scroll_box(width = "700px")

###################################################
library(readxl)
library(kableExtra)

com_prog<- read_xlsx("Comunidades_Programadas_2024_03_07-2024.xlsx")

com_prog |>
  filter(Tipo != c("Asociación", "Comité")) |>
  kable(com_prog, format = "markdown", escape = FALSE, align = c('c', 'c')) |>
    row_spec(0, bold = TRUE, background = "#CCCCFF") |>
    kable_styling("striped", full_width = F) |>
    scroll_box(height ="400px",   width = "600px")
















