


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
Palm_Param = compute_archi(
    map = map, data_path = "0-data/Archi",
    write_path = "../VPalm_Architecture/models_MAP_59.RData"
)
# Palm_Param= readRDS(file = "../VPalm_Architecture/models_MAP_47.RData")
scene = make_scene(
    data = Palm_Param, ntrees = ntrees, nleaves = nleaves,
    path = "3-Outputs",
    AMAPStudio = getwd(),
    plant_dist = plant_dist
)

scene = make_scene(
    data = Palm_Param, ntrees = NULL, nleaves = 45,
    path = "../VPalm_Architecture/test", Progeny = "DA1",
    AMAPStudio = getwd(), seed = NULL,
    plant_dist = plant_dist
)


map= 47
plant_dist= 9.2
plot_design= NULL
overwrite= T
seed = 1:4
progress= NULL
ntrees = 4
data = Palm_Param
nleaves = 45
path = "3-Outputs/Tests"
Progeny = "DA1"
AMAPStudio = getwd()
