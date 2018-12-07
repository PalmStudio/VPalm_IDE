


# Sourcing ----------------------------------------------------------------

rm(list = ls())
library(Vpalmr)


# Initializing ------------------------------------------------------------

# ntrees= 0
ntrees= NULL
nleaves= 45
map= 47

# Import all data and fit the models:
# Palm_Param= compute_archi(map = map, data_path = "1-Data/Archi", write_path = "4-Outputs")
Palm_Param= readRDS(file = "3-Outputs/models.RData")
scene= make_scene(data = Palm_Param, ntrees = ntrees, nleaves = nleaves, 
                  path = "4-Outputs_new", 
                  AMAPStudio = "2-VPalm_exe",
                  plant_dist = 9.2)
