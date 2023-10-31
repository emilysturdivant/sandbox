# Prepare land tenure polygons for LCI validation analysis
# Follow categories used in Kruid, et al. 2021

library(tidyverse)
library(sf)
sf_use_s2(FALSE) # Turn off spherical geometry (https://r-spatial.org/book/04-Spherical.html#validity-on-the-sphere)
library(tmap)
tmap_mode('view')

all <- sf::st_read('/Users/esturdivant/data/lci/raisg/msk_anps_tis_all.shp') %>% 
  filter(pais=='Brasil') %>% 
  st_make_valid()
# all %>% tbl_vars()
# all %>% st_drop_geometry() %>% count(tis_0_cat, tis_0_leg, tis_priorl)

its <- all %>% filter(!is.na(tis_0_cat))
# its %>% st_drop_geometry() %>% count(dep_0_cat, dep_0_leg)
# its %>% st_drop_geometry() %>% count(nac_0_cat, nac_0_leg)
# its %>% st_drop_geometry() %>% count(nac_1_cat, nac_1_leg)
# its %>% st_drop_geometry() %>% count(dep_0_cat, nac_0_cat, nac_1_cat, nac_2_cat)
its_excl <- its %>% filter(is.na(dep_0_cat) & is.na(nac_0_cat) & is.na(nac_1_cat) & is.na(nac_2_cat))
itpa_overlap <- its %>% filter(!is.na(dep_0_cat) | !is.na(nac_0_cat) | !is.na(nac_1_cat) | !is.na(nac_2_cat))
# itpa_overlap <- its_excl %>% anti_join(st_drop_geometry(its))

sans_it <- all %>% anti_join(st_drop_geometry(its_excl))
# sans_it %>% st_drop_geometry() %>% count(dep_2_cat, dep_2_leg)
# sans_it %>% st_drop_geometry() %>% count(dep_1_cat, dep_1_leg)
# sans_it %>% st_drop_geometry() %>% count(dep_0_cat, dep_0_leg)
# sans_it %>% st_drop_geometry() %>% count(nac_0_cat, nac_0_leg)
# sans_it %>% st_drop_geometry() %>% count(nac_1_cat, nac_1_leg)
# sans_it %>% st_drop_geometry() %>% count(nac_2_cat, nac_2_leg)
# sans_it %>% st_drop_geometry() %>% count(nac_3_cat, nac_3_leg)
# sans_it %>% st_drop_geometry() %>% count(nac_4_cat, nac_4_leg)
# sans_it %>% st_drop_geometry() %>% count(BOS_0_cat, BOS_0_leg)
sans_pna <- sans_it %>% 
  filter(is.na(dep_0_cat) & is.na(dep_1_cat) & is.na(dep_2_cat) & 
           is.na(nac_0_cat) & is.na(nac_1_cat) & is.na(nac_2_cat))

other_land <- sans_pna
other_land %>% qtm()





pnas <- all %>% 
  mutate(it_flag = ifelse(!is.na(tis_priorl), 'TI', NA)) %>% 
  mutate(pna_flag = case_when(
           str_detect(nac_0_cat, 
                      str_c('Estação Ecológica', 'Parque Nacional', collapse='|')) ~ 'PNA', 
         TRUE ~ as.character(NA))) #%>% 
  # mutate(tenure =
  #          case_when(
  #            !(is.na(tis_priorl)) ~ 'TI', 
  #            str_detect(nac_0_cat, 
  #                       str_c('Estação Ecológica', 'Parque Nacional', collapse='|')) ~ 'PNA',
  #            TRUE ~ as.character(NA)))

pnas %>% filter(tenure == 'PNA') %>% qtm()




# TI and PA boundaries used for Walker et al. 2020 ---- 
fc <- sf::st_read("/Users/esturdivant/data/lci/raisg/msk_paises.gdb")

# 1 = IT; 2 = PNA; 3 = IT/PNA Overlap; 4 = Other Land
fc %>% st_drop_geometry() %>% count(TIPO_UND)

# Files from Brazilian forestry service ----
fps <- list.files('/Users/esturdivant/data/lci/raisg/CNFP 2020 Shapefiles (5)', 
                  '.shp', full.names=TRUE)
amzn <- st_read(fps[3])
amzn %>% tbl_vars()
amzn %>% st_drop_geometry() %>% distinct(classe)
amzn %>% st_drop_geometry() %>% distinct(estagio)
amzn %>% st_drop_geometry() %>% distinct(governo)
amzn %>% st_drop_geometry() %>% distinct(protecao)
amzn %>% st_drop_geometry() %>% distinct(tipo)
amzn %>% st_drop_geometry() %>% distinct(comunitari)
amzn %>% st_drop_geometry() %>% distinct(categoria)

amzn %>% 
  st_drop_geometry() %>% 
  distinct(classe, estagio, governo, protecao, tipo, comunitari, categoria)

amzn %>% st_drop_geometry() %>% distinct(tipo)
