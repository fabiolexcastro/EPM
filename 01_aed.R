
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, readxl, qdap, hablar)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999,
        stringsAsFactors = FALSE)

# Load data ---------------------------------------------------------------
shp <- st_read('../shp/base/bcs_manzanas_comunas.shp')
com <- st_read('../shp/base/bcs_comunas_geo.shp') %>% 
  mutate(COMUNA = as.numeric(as.character(COMUNA)))
tbl <- read_excel('../tbl/0916_percepcion.xlsx')
tbl <- tbl[-1,]
tbl <- tbl %>% 
  mutate(id_man = as.numeric(id_man))

# Join between the table and the shapefile
mnz <- shp %>% 
  as.data.frame %>% 
  as_tibble %>% 
  dplyr::select(IDMANZANA, COMUNA, NOMBRE)
tbl <- inner_join(tbl, mnz, by = c('id_man' = 'IDMANZANA'))

# Mapa 1. Usted se reconoce cómo ------------------------------------------
m01 <- function(){
  print('Mapa 01')
  rsl <- tbl %>% 
    dplyr::select(COMUNA, b_2) %>% 
    group_by(COMUNA, b_2) %>% 
    dplyr::summarize(count = n()) %>% 
    ungroup() %>% 
    spread(b_2, count) %>% 
    setNames(c('COMUNA', 'cicl', 'peaton', 'tpriv', 'tpubl')) %>% 
    NAer() 
  rsl <- inner_join(com, rsl, by = c('COMUNA'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = 'm01_tipoPersona', driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
m01()

# Mapa 2. Modo de transporte habitual (viajes largos) ---------------------
m02 <- function(){
  print('Mapa 02')
  rsl <- tbl %>% 
    dplyr::select(COMUNA, b_4) %>% 
    group_by(COMUNA, b_4) %>% 
    dplyr::summarize(count = n()) %>% 
    ungroup() %>% 
    spread(b_4, count) %>% 
    setNames(c('COMUNA', 'pie', 'cicl', 'bus', 'MIO', 'moto', 'taxicolec', 'taxi', 'tranp_inf', 'vpart')) %>% 
    NAer() %>% 
    as_tibble()
  rsl <- inner_join(com, rsl, by = c('COMUNA'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = 'm02_transpHabitual', driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
m02()

# -------------------------------------------------------------------------
# Peaton ------------------------------------------------------------------
# -------------------------------------------------------------------------

# Mapa 3. Calificacion infraestructura para peatones ----------------------
m03 <- function(){
  print('Mapa 03')
  rsl <- tbl %>% 
    dplyr::select(COMUNA, d_7) %>% 
    mutate(d_7 = as.numeric(d_7)) %>% 
    group_by(COMUNA) %>% 
    dplyr::summarize(avg = mean(d_7, na.rm = TRUE)) %>% 
    ungroup()
  rsl <- inner_join(com, rsl, by = c('COMUNA'))  
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = 'm03_infrasPeaton', driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
m03()

# Mapa 4. Seguridad para peatones -----------------------------------------
m04 <- function(){
  print('Mapa 04')
  rsl <- tbl %>% 
    dplyr::select(COMUNA, d_13) %>% 
    mutate(d_13 = as.numeric(d_13)) %>% 
    group_by(COMUNA) %>% 
    dplyr::summarize(avg = mean(d_13, na.rm = TRUE)) %>% 
    ungroup()
  rsl <- inner_join(com, rsl, by = c('COMUNA'))  
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = 'm04_seguridadPeaton', driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
m04()

# Mapa 5. Seguridad accidentes peaton -------------------------------------
m05 <- function(){
  print('Mapa 05')
  rsl <- tbl %>% 
    dplyr::select(COMUNA, d_14) %>% 
    mutate(d_14 = as.numeric(d_14)) %>% 
    group_by(COMUNA) %>% 
    dplyr::summarize(avg = mean(d_14, na.rm = TRUE)) %>% 
    ungroup()
  rsl <- inner_join(com, rsl, by = c('COMUNA'))  
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = 'm05_accidentesPeaton', driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
m05()

# Mapa 6. Satisfaccion calidad infraestructura peaton ---------------------
m06 <- function(){
  print('Mapa 06')
  rsl <- tbl %>% 
    dplyr::select(COMUNA, d_17) %>% 
    mutate(d_17 = as.numeric(d_17)) %>% 
    group_by(COMUNA) %>% 
    dplyr::summarize(avg = mean(d_17, na.rm = TRUE)) %>% 
    ungroup()
  rsl <- inner_join(com, rsl, by = c('COMUNA'))  
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = 'm06_satisfaccionPeaton', driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
m06()

# -------------------------------------------------------------------------
# Ciclistas ---------------------------------------------------------------
# -------------------------------------------------------------------------

# Mapa 7. Infraestructura movilidad bicicletas ----------------------------
m07 <- function(){
  print('Mapa 07')
  rsl <- tbl %>% 
    dplyr::select(COMUNA, c_7) %>% 
    mutate(c_7 = as.numeric(c_7)) %>% 
    group_by(COMUNA) %>% 
    dplyr::summarize(avg = mean(c_7, na.rm = TRUE)) %>% 
    ungroup()
  rsl <- inner_join(com, rsl, by = c('COMUNA'))  
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = 'm07_infrasCiclista', driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
m07()

# Mapa 8. Seguridad robos bicicletas --------------------------------------
m08 <- function(){
  print('Mapa 08')
  rsl <- tbl %>% 
    dplyr::select(COMUNA, c_13) %>% 
    mutate(c_13 = as.numeric(c_13)) %>% 
    group_by(COMUNA) %>% 
    dplyr::summarize(avg = mean(c_13, na.rm = TRUE)) %>% 
    ungroup()
  rsl <- inner_join(com, rsl, by = c('COMUNA'))  
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = 'm08_seguridadCiclista', driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
m08()

# Mapa 9. Seguridad accidentes bicicleta ----------------------------------
m09 <- function(){
  print('Mapa 09')
  rsl <- tbl %>% 
    dplyr::select(COMUNA, c_14) %>% 
    mutate(c_13 = as.numeric(c_14)) %>% 
    group_by(COMUNA) %>% 
    dplyr::summarize(avg = mean(c_14, na.rm = TRUE)) %>% 
    ungroup()
  rsl <- inner_join(com, rsl, by = c('COMUNA'))  
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = 'm09_seguaccidentesCiclista', driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
m09()

# Mapa 10. Infraestructura movilidad bicicletas ---------------------------
m10 <- function(){
  print('Mapa 10')
  rsl <- tbl %>% 
    dplyr::select(COMUNA, c_19) %>% 
    mutate(c_19 = as.numeric(c_19)) %>% 
    group_by(COMUNA) %>% 
    dplyr::summarize(avg = mean(c_19, na.rm = TRUE)) %>% 
    ungroup()
  rsl <- inner_join(com, rsl, by = c('COMUNA'))  
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = 'm10_calidadInfraestrCiclista', driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
m10()

# -------------------------------------------------------------------------
# General -----------------------------------------------------------------
# -------------------------------------------------------------------------

# Mapa 11. Satisfaccion general -------------------------------------------
m11 <- function(){
  print('Mapa 11')
  rsl <- tbl %>% 
    dplyr::select(COMUNA, f_0) %>% 
    group_by(COMUNA, f_0) %>% 
    dplyr::summarize(count = n()) %>% 
    ungroup() %>% 
    spread(f_0, count) %>% 
    setNames(c('COMUNA', 'insatisf', 'satisf')) %>% 
    ungroup()
  rsl <- inner_join(com, rsl, by = c('COMUNA'))  
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = 'm11_satisfaccionGral', driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
m11()

# Mapa 12. Disposicion a caminar ------------------------------------------
m12 <- function(){
  print('Mapa 12')
  rsl <- tbl %>% 
    dplyr::select(COMUNA, f_1) %>% 
    mutate(f_1 = as.numeric(f_1)) %>% 
    group_by(COMUNA) %>% 
    dplyr::summarize(avg = mean(f_1, na.rm = TRUE)) %>% 
    ungroup()
  rsl <- inner_join(com, rsl, by = c('COMUNA'))  
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = 'm12_disposicionCaminar', driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
m12()

# Mapa 13. Disposicion bicicleta ------------------------------------------
m13 <- function(){
  print('Mapa 13')
  rsl <- tbl %>% 
    dplyr::select(COMUNA, f_3) %>% 
    mutate(f_3 = as.numeric(f_3)) %>% 
    group_by(COMUNA) %>% 
    dplyr::summarize(avg = mean(f_3, na.rm = TRUE)) %>% 
    ungroup()
  rsl <- inner_join(com, rsl, by = c('COMUNA'))  
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = 'm13_disposicionBicicleta', driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
m13()





