


# Sourcing ----------------------------------------------------------------

rm(list = ls())
library(Vpalmr)


# Initializing ------------------------------------------------------------

# ntrees= 0
ntrees= NULL
nleaves= 45
map= 47
plant_dist= 9.2

# Import all data and fit the models:
# Palm_Param= compute_archi(map = map, data_path = "1-Data/Archi", write_path = "3-Outputs")
Palm_Param= readRDS(file = "3-Outputs/models_MAP_47.RData")
# scene= make_scene(data = Palm_Param, ntrees = ntrees, nleaves = nleaves, 
#                   path = "3-Outputs", 
#                   AMAPStudio = "2-VPalm_exe",
#                   plant_dist = plant_dist)

scene= make_scene(data = Palm_Param, ntrees = 0, nleaves = 45, 
                  path = "3-Outputs", Progeny = "DA1",
                  AMAPStudio = "2-VPalm_exe",
                  plant_dist = plant_dist)


