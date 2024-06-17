##########################################
# generar tablas y gráficos descriptivos #
# para el reporte trimestral             #
##########################################

# especificar la ruta
getwd()
# abrir los datos: " smart_data_t1.RDA "
setwd("U:/Reporte_PNCAZ/rda")
dir()

load("smart_data_t1.RDA")
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
save(tabla_sinhtml, file="U:/Reporte_PNCAZ/rda/tabla_sinhtml.RDA")
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
input_file <- "U:/Reporte_PNCAZ/quarto_template/estructura_programa.html"
output_file <- "U:/Reporte_PNCAZ/quarto_template/estructura_programa.docx"

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

############################

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
implementados %>%
  mutate(Hito = paste0("[", Hito, "](", Link, ")")) %>%
  select(-Link) %>%  # Eliminar la columna de URLs
  kable(format = "markdown", escape = F) %>%
  kable_styling("striped", full_width = F) %>%
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







