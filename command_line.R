


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
Palm_Param= compute_archi(map = map, data_path = "1-Data/Archi", write_path = "3-Outputs")
Palm_Param= readRDS(file = "3-Outputs/models_MAP_47.RData")
scene= make_scene(data = Palm_Param, ntrees = ntrees, nleaves = nleaves, 
                  path = "3-Outputs", 
                  AMAPStudio = "2-VPalm_exe",
                  plant_dist = plant_dist)




test= data.table::fread('1-Data/Archi/Torsion_SMSE14.csv', dec= ',', data.table= F)

scene= make_scene(data = Palm_Param, ntrees = 0, nleaves = 45, 
                  path = "3-Outputs", Progeny = NULL,
                  AMAPStudio = "2-VPalm_exe",
                  plant_dist = plant_dist)


scene= make_scene(data = Palm_Param, ntrees = 0, nleaves = 45, 
                  path = "3-Outputs", Progeny = "DA1",
                  AMAPStudio = "2-VPalm_exe",
                  plant_dist = plant_dist)

scene$plot_design%>%
  mutate(image= 'www/palm_tiny.png')%>%
  ggplot(aes(x= x, y= y))+
  geom_image(aes(image= image), size= 0.4)+
  geom_point(aes(color= "Palm tree center"))+
  ylim(low= unique(plot_design$ymin),
       high= unique(plot_design$ymax))+
  xlim(low= unique(plot_design$xmin),
       high= unique(plot_design$xmax))+
  labs(colour = "")+
  theme(legend.position="bottom")
