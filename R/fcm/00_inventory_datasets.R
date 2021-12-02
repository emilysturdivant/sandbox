#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Work with inventory table of datasets
# Author:
#     * esturdivant@woodwellclimate.org, 2021-11-30
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load libraries 
library(sf)
library(colorspace)
library(tidyverse)

data_dir <- '/Users/emilysturdivant/data'

# Load spreadsheet
csv <- here::here(data_dir, 'fci_inventory', 'fci_datasets_inventory.csv')
tbl <- read_csv(csv)

# Filter
tbl %>% filter(Relevance == 'biophysical') %>% distinct(`Sub-category`)
tbl %>% tbl_vars()
tbl %>% 
  filter(`Frequency of updates (years)` < 5 | is.na(`Frequency of updates (years)`)) %>% 
  filter(`Sub-category` == 'biodiversity') %>% 
  select(`Variable name`, `Resolution (at the equator)`, 
         `Frequency of updates (years)`, `Date`, `Used for`, `Coverage`, `Access URL`)
  
tbl %>% 
  select(Relevance:Coverage) %>% 
  filter(str_detect(`Variable name`, 
                    regex('baccini 500', ignore_case = TRUE)) |
         str_detect(`Variable name`, 
                    regex('forest landscape integrity index', ignore_case = TRUE)),
         )

download.file('http://dare.iiasa.ac.at/57/1/BendingTheCurve_supportingMaterial_final_28August2020.zip',
              here::here(data_dir, 'BendingTheCurve_supportingMaterial_final_28August2020.zip'))
