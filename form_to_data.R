# Purpose: to import the data from the field forms and to format it as VPalm inputs
# Date: 20/20/2019


# Imports -----------------------------------------------------------------

library(data.table)
library(xlsx)
library(lubridate)
library(tidyverse)
library(Vpalmr)

archi= Vpalmr::extract_sheets(path = "0-data/raw/Form Archi.xlsx")
fwrite(archi, "0-data/processed/update_archi.csv", sep=";")










