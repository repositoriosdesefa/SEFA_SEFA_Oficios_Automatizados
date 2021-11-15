####################################################################-
########  Generación automatizada  de oficios reiterativos  ########-
############################# By LE ################################-

##################### 0. Parámetros generales #####################-

# 0.1 Parámtros ----
# Parámetros Locales
DIRECTORIO <- "C:/Users/48393642/Proyectos_SEFA"
PROYECTO <- file.path(DIRECTORIO, "Oficios_Auto/Word")
MODELO_RMD <- file.path(PROYECTO, "Abstraccion.Rmd")
# Parámetros No Locales (En la web)
NOMBRE_PEDIDO <- "PEDIDO"
M_INSUMOS <- ""
TABLA_INSUMOS <- "INSUMOS"


################ I. Librerias, drivers y directorio ################
# I.1 Librerias ----
library(dplyr)
library(readxl)
library(rmarkdown)
library(purrr)
library(lubridate)
library(googledrive)
library(googlesheets4)
library(stringi)

# I.2 Parametros ----
# i) Conexion de la base y descarga de matriz de insumos
tp1 <- tempfile() # Creacion de un archivo temporal
download.file(M_INSUMOS, tp1, mode ="wb")

########################### II. Funciones ###########################
# II.1 Funcion de renderizado de documento ----
auto_lec_rep <- function(ht, num, lugar, nombre, 
                         efa, defa, prefa, direfa,
                         oci, doci, proci, diroci, 
                         firma, aux_1, aux_2, aux_3){

  # Se eliminan carácteres especiales
  efa_n = gsub("Ñ", "N", efa)
  
  # Eliminación de tildes
  con_tilde_may <- c("Á", "É", "Í", "Ó", "Ú")
  sin_tilde_may <- c("A", "E", "I", "O", "U")
  con_tilde_min <- c("á", "é", "í", "ó", "ú")
  sin_tilde_min <- c("a", "e", "i", "o", "u")
  
  efa_f = stri_replace_all_regex(efa_n, con_tilde_may, sin_tilde_may, vectorize = F)
  efa_f = stri_replace_all_regex(efa_f, con_tilde_min, sin_tilde_min, vectorize = F)
  
  rmarkdown::render(input = MODELO_RMD,
                    # Heredamos los par?metros desde la matriz de insumos
                    params = list(HT_1 = ht,
                                  N_OFICIO_2 = num,
                                  LUGAR_3 = lugar,
                                  DESTINATARIO_4 = nombre,
                                  EFA_5 = efa,
                                  DPTO_EFA_6 = defa,
                                  PROV_EFA_7 = prefa,
                                  DIR_EFA_8 = direfa,
                                  OCI_9 = oci,
                                  DPTO_OCI_10 = doci,
                                  PROV_OCI_11 = proci,
                                  DIR_OCI_12 = diroci,
                                  FIRMANTE_13 = firma,
                                  ADICIONAL_14 = aux_1,
                                  ADICIONAL_15 = aux_2,
                                  ADICIONAL_16 = aux_3),
                    output_file = paste0(NOMBRE_PEDIDO,
                                         " - ",
                                         efa_f))
}

# II.2 Funcion robustecida -----
R_auto_lec_rep <- function(ht, num, lugar, nombre, 
                           efa, defa, prefa, direfa,
                           oci, doci, proci, diroci, 
                           firma, aux_1, aux_2, aux_3){
  out = tryCatch(auto_lec_rep(ht, num, lugar, nombre, 
                              efa, defa, prefa, direfa,
                              oci, doci, proci, diroci, 
                              firma, aux_1, aux_2, aux_3),
                 error = function(e){
                   auto_lec_rep(ht, num, lugar, nombre, 
                                efa, defa, prefa, direfa,
                                oci, doci, proci, diroci, 
                                firma, aux_1, aux_2, aux_3) 
                 })
  return(out)
}

###################### III. Procesamiento de datos ######################
# Carga de datos
INSUMOS <- as.data.frame(read_xlsx(tp1, sheet = TABLA_INSUMOS))
# Filtrado
INSUMOS <- INSUMOS %>%
  filter(DESTINATARIO == "SI")

########################### III. Renderizado ###########################
# III.1 Creación del documento ----
pwalk(list(INSUMOS$HT_1, 
           INSUMOS$N_OFICIO_2,
           INSUMOS$LUGAR_3,
           INSUMOS$DESTINATARIO_4,
           INSUMOS$EFA_5, 
           INSUMOS$DPTO_EFA_6,
           INSUMOS$PROV_EFA_7,
           INSUMOS$DIR_EFA_8,
           INSUMOS$OCI_9, 
           INSUMOS$DPTO_OCI_10,
           INSUMOS$PROV_OCI_11,
           INSUMOS$DIR_OCI_12,
           INSUMOS$FIRMANTE_13,
           INSUMOS$ADICIONAL_14,
           INSUMOS$ADICIONAL_15,
           INSUMOS$ADICIONAL_16),
      slowly(R_auto_lec_rep, 
             rate_backoff(10, max_times = Inf)))
