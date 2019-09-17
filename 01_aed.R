

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, readxl)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
makeMean <- function(col){
  # col <- 'd_1'
  x <- tbl[,c('id_man', col)] %>% 
    setNames(c('id_man', 'var')) %>% 
    mutate(id_man = as.numeric(id_man),
           var = as.numeric(var))
  x <- inner_join(shp, x, by = c('IDMANZANA' = 'id_man')) %>% 
    dplyr::select(IDMANZANA, COMUNA, NOMBRE, var)
  x <- x %>% 
    as.data.frame %>% 
    dplyr::select(-geometry) %>% 
    as_tibble %>% 
    group_by(COMUNA) %>% 
    dplyr::summarise(var = mean(var, na.rm = TRUE)) %>% 
    ungroup()
  y <- inner_join(com, x, by = c('COMUNA' = 'COMUNA'))
  print('Done!')
  return(y) 
}

# Load data ---------------------------------------------------------------
shp <- st_read('../shp/base/bcs_manzanas_comunas.shp')
com <- st_read('../shp/base/bcs_comunas_geo.shp') %>% 
  mutate(COMUNA = as.numeric(as.character(COMUNA)))
tbl <- read_excel('../tbl/0916_percepcion.xlsx')
tbl <- tbl[-1,]

# Tipo de usuario
smm_type <- tbl %>% 
  dplyr::select(id_man, b_2) %>% 
  pull() %>% 
  table %>% 
  as.data.frame() %>% 
  setNames(c('Type', 'Freq'))

# Peatones
ptn <- tbl %>% 
  filter(b_2 == 'Peatón')

# Ciclistas
ccl <- tbl %>% 
  filter(b_2 == 'Ciclista')

# Mapas en relacion al peaton ---------------------------------------------
vrs_ptn <- c('d_1', 'd_2', 'd_3', 'd_5', paste0('d_', 7:17))
shp_ptn <- map(.x = vrs_ptn, .f = makeMean)

# Mapas en relacion al ciclista -------------------------------------------
vrs_ccl <- c('c_1', 'c_2', 'c_3', 'c_5', paste0('c_', 7:19))
shp_ccl <- map(.x = vrs_ccl, .f = makeMean)


# Escritura de shapes -----------------------------------------------------






