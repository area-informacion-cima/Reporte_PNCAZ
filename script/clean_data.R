##################################
# Datos de Vigilancia y Control  #
# del PNCAZ - SMART              #
##################################
rm(list = ls())
## instalar paquetes y abrir librerías##
# install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
# install.packages('modelsummary')
library(modelsummary)

######### Organizacion y limpieza de datos #######

#### Patrullajes ####
## 1. importar base de datos csv 
# Esta fue extraída del smart
setwd("U:/Reporte_PNCAZ/datos")
dir()

smart_d<-read.csv(file = "01_SMART_registro_actividades_humanas_000102_4.csv", header = T, sep = ",",stringsAsFactors = )
glimpse(smart_d)

# renombrar las variables
names(smart_d)
class(smart_d) #  convertir a formato tibble
datos <- as.tibble(smart_d)

##### conjunto de datos con nuevos nombres ####
datos<-smart_d |>
  rename(
    id_patrullaje = ID.de.Patrullaje.,
    tipo_desplazamiento = Tipo.,
    inicio_patrullaje = Fecha.de.Inicio.de.Patrullaje.,
    final_patrullaje = Fecha.de.finalizacion.de.Patrullaje.,
    pvc_pncaz = Estacion.,
    objetivo_pncaz = Objetivo.,
    tipo_patrullaje = Mandato.,
    lider_patrullaje = Lider,
    conductor_patrullaje = Piloto.,
    transporte_patrullaje = Tipo.de.Transporte.de.Patrullaje.,
    id_coord = ID.de.coordenada.,
    fecha_coord = Fecha.de.coordenada.,
    hora_patrullaje = Hora.,
    x_coord = X,
    y_coord = Y,
    coment_patrullaje = Comentario.,
    ultima_modif = Ultima.Modificacion,
    tipo_obs = geometria.de.Observacion.0,
    tipo_obs1 = geometria.de.Observacion.1,
    cargo_personal = X0_Cargo_personal, #20
    actor_patrullaje = `X0_InstituciÃ³n_personal`,
    nombre_personal = X0_Nombre.de.persona_personal,
    combustible_cantidad = X01_Cantidad.de.combustible,
    combustible_costo = X01_Costo.de.combustible.por.unidad,
    combustible_tipo = X01_Tipo.de.combustible,
    alerta_def = Alerta.de.deforestacion,
    salud_animal = Anomalias.en.Animales.Enfermos..Lesionados..o.Muertos,
    area_def = Area.afectada..hectÃ.reas.,
    fauna_amenaza = CategorÃ.a.de.amenaza...fauna,
    flora_amenaza = CategorÃ.a.de.amenaza...flora,
    efecto_actividad = Categoria.de.efecto,
    sp_cultivada1 = Describir.otra.especie.cultivada,
    tipo_infraccion = `DescripciÃ³n.de.infracciÃ³n`,
    fauna_sector = `DescripciÃ³n.del.lugar...fauna`,
    dni_intervencion = DNI,
    edad_fauna = Edad...fauna,
    sp_cultivada = Especie.cultivada,
    fenologia_flora = FenologÃ.a,
    fuente_patrullaje = Fuente.de.financiamiento,
    gps_patrullaje = GPS,
    act_humanas = Lista.de.actividades,
    efect_humanas = Lista.de.efectos,
    nombre_sector = Lugar...Sector,
    sp_caza = Nombre.cientÃ.fico...caza,
    sp_fauna = Nombre.cientÃ.fico...fauna,
    sp_flora = Nombre.cientÃ.fico...flora,
    sp_forestal = Nombre.cientÃ.fico...forestal, # urante los registro de actividades humanas
    nombre_caza = Nombre.comÃºn...caza,
    nombre_fauna = Nombre.comÃºn...fauna,
    nombre_flora = Nombre.comÃºn...flora,#30
    nombre_forestal = Nombre.comÃºn...forestal,
    nombre_intervenido = Nombre.de.persona.intervenida,
    num_sanos_fauna = Numero.Animales.Sanos,
    num_hembras_fauna = NÃºmero.de.hembras...fauna,
    num_ind = NÃºmero.de.individuos,
    num_juv_fauna = NÃºmero.de.juveniles.crÃ.as...fauna,            
    num_machos_fauna = NÃºmero.de.machos...fauna,     
    num_noident = NÃºmero.de.no.identificados...fauna,
    otras_sp_fauna = Otra.especie...fauna,
    otras_sp_flora = Otra.especie...flora,
    comentario_1 = Otra.Informacion.Relevante,
    equipos_patrullaje = Otros.equipos,
    parte_forestal = Parte.aprovechada...forestal,
    posicion_patrullaje = `PosiciÃ³n.del.Patrullaje`,
    trocha_patrullaje = Tipo.de.actividad...transporte,
    tipo_ganado = Tipo.de.ganado,
    registro_fauna = Tipo.de.registro...fauna,
    total_ind = Total.de.individuos...fauna #18
) # 97-29 numero de columnas en el conjunto de datos =  68

## 2. seleccionar variables a utilizar en los reportes trimestrales

datos<- datos |>
  select(
    contains("_"),
    -matches("\\.")) # seleccionar menos las variables que contienen " . "
names(datos)
datos <- as.tibble(datos)
####### asignar formato a las variables #######
glimpse(datos)

########## variables : factores ############

#### tipo de desplazamiento: acuático, terrestre####
datos$tipo_desplazamiento <- as.factor(datos$tipo_desplazamiento)



#### puestos de vigilancia: renombrar lo siguiente: ####
# "PV San JosÃ© de Yanayacu"
# "PV PÃ³lvora" 

datos$pvc_pncaz[datos$pvc_pncaz == "PV San JosÃ© de Yanayacu"] <- "PV San Jose de Yanayacu"
datos$pvc_pncaz[datos$pvc_pncaz == "PV PÃ³lvora"] <- "PV Polvora"
class(datos$pvc_pncaz)



#### tipo de patrullaje: rutinario, vigilancia comunal, especial ####
unique(datos$tipo_patrullaje)
n_orig <- unique(datos$tipo_patrullaje)
n_new  <- c("rutinario", "especial", "vigilancia_comunal")

datos$tipo_patrullaje <- recode(datos$tipo_patrullaje, 
                                !!!setNames(n_new, n_orig))
class(datos$tipo_patrullaje)


#### tipo de observación ####
datos$tipo_obs[datos$tipo_obs==""]<- "s/i"
n_orig <- unique(datos$tipo_obs)
n_new  <- c("posicion", "flora_priorizada", "s/i", 
            "equipo_patrullaje","fauna_priorizada", 
            "act_humanas")

datos$tipo_obs <- recode(datos$tipo_obs, !!!setNames(n_new, n_orig))
unique(datos$tipo_obs)


####tipo observación_1 ####
datos$tipo_obs1[datos$tipo_obs1==""] <- "s/i"

n_orig <- unique(datos$tipo_obs1)
n_new  <- c("s/i", "personal_adicional", "equipo_utilizado", 
            "asignacion_recursos", "28_agricultura", "31_extraccion_fauna",
            "29_ganaderia","30_extraccion_forestal","37_transporte"
            ,"40_otras_actividades" )

datos$tipo_obs1 <- recode(datos$tipo_obs1, !!!setNames(n_new, n_orig))
unique(datos$tipo_obs1)

datos$tipo_obs1 <- as.factor(datos$tipo_obs1)
unique(datos$tipo_obs1)


#### alerta deforestación ####
unique(datos$alerta_def)
datos$alerta_def[datos$alerta_def == ""] <- "s/i"


#### fauna amenazada ####
unique(datos$fauna_amenaza)
datos$fauna_amenaza[datos$fauna_amenaza == ""] <- "s/i"

n_orig <- unique(datos$fauna_amenaza)
n_new  <- c("s/i", "VU", "NT", "DD", "ND", "EN", "CR")

datos$fauna_amenaza <- recode(datos$fauna_amenaza, 
                              !!!setNames(n_new, n_orig))

#### flora amenazada ####

unique(datos$flora_amenaza)

datos$flora_amenaza[datos$flora_amenaza == ""] <- NA
datos$flora_amenaza[datos$flora_amenaza == "Vulnerable-VU"] <- "VU"
datos$flora_amenaza[datos$flora_amenaza == "No determinado"] <- "ND"
datos$flora_amenaza <- as.factor(datos$flora_amenaza)


#### registro de fauna ####
unique(datos$registro_fauna)
datos$registro_fauna[datos$registro_fauna == ""] <- "s/i"

n_orig <- unique(datos$registro_fauna)
n_new  <- c("s/i", "visual", "huellas", "auditivo", "otros", "aranhazos",
            "comederos", "heces", "nidos", "rastros")

datos$registro_fauna <- recode(datos$registro_fauna,
                               !!!setNames(n_new, n_orig))

#### dar formato a las fechas ####
# inicio de patrullaje
unique(datos$inicio_patrullaje)
# Definir un vector de nombres de meses abreviados en español
meses <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
## Reemplazar el mes abreviado por su número correspondiente ##
for (i in 1:length(meses)) {
  datos$inicio_patrullaje <- gsub(meses[i], sprintf("%02d", i), datos$inicio_patrullaje)
}
# Eliminar el punto después del mes #
datos$inicio_patrullaje <- gsub("\\.", "", datos$inicio_patrullaje)
# Convertirlo a formato de fecha
datos$inicio_patrullaje<- as.Date(datos$inicio_patrullaje, format = "%d %m %Y")

##### final de patrullaje
unique(datos$final_patrullaje)
## Reemplazar el mes abreviado por su número correspondiente ##
for (i in 1:length(meses)) {
  datos$final_patrullaje <- gsub(meses[i], sprintf("%02d", i), datos$final_patrullaje)
}
# Eliminar el punto después del mes #
datos$final_patrullaje <- gsub("\\.", "", datos$final_patrullaje)
# Convertirlo a formato de fecha
datos$final_patrullaje<- as.Date(datos$final_patrullaje, format = "%d %m %Y")


#### fecha de coordenada #####
datos$fecha_coord
# Definir un vector de nombres de meses abreviados en español
meses <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")

## Reemplazar el mes abreviado por su número correspondiente ##
for (i in 1:length(meses)) {
  datos$fecha_coord <- gsub(meses[i], sprintf("%02.d", i), datos$fecha_coord)
}
# Eliminar el punto después del mes #
datos$fecha_coord <- gsub("\\.", "", datos$fecha_coord)
# Convertir a formato de fecha
datos$fecha_coord <- as.Date(datos$fecha_coord, format = "%d %d %Y")


#### posicion de patrullaje ####
unique(datos$posicion_patrullaje)
# Vector con los nombres originales
datos$posicion_patrullaje[datos$posicion_patrullaje == ""] <-"s/i"
n_orig <- unique(datos$posicion_patrullaje)

# Vector con los nombres nuevos
n_new <- c("fin", "reinicio","parada", "destino", 
           "s/i", "inicio","posicion")

# Cambiar los nombres automáticamente
datos$posicion_patrullaje <- recode(datos$posicion_patrullaje, 
                                    !!!setNames(n_new, n_orig))

# Verificar los nombres cambiados
unique(datos$posicion_patrullaje)


############# Guardar conjunto de datos ##############################################
# 1 establecer la ruta y carpeta la carpeta donde se guardará el conjunto de datos:
# establecer ruta
getwd()
setwd("U:/Reporte_PNCAZ/rda")
# crear la carpeta "rda"
dir.create(file.path(getwd(), "rda"))
dir()
# guardar los datos como objeto RDA
save(datos, file = "smart_data_t1.RDA")

#####################################################################################

# Limpiar la consola y el "Environment" 
# rm(list = ls())


### LOAD resumen datos de Patrullaje ####
setwd("U:/Reporte_PNCAZ/datos")
dir()
resumen <- read.csv("resumen_puestos_patrullaje_T1.csv", header = T, sep = ";")
resumen |>
  group_by(PVC)

knitr::kable(resumen, caption = "Tabla de ejemplo")
  




datasummary_skim(test_4, type = "categorical", 
                 output = "markdown") # tabla 1









