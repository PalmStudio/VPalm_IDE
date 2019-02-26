# Purpose: update the architectural data
# Date: 19/02/2019
# Author: R. Vezy




# Loading -----------------------------------------------------------------

library(data.table)
library(Vpalmr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)


# Importing the files -----------------------------------------------------

parameters= fread("1-Data/Archi/ParameterSimu.csv", data.table = F)

development= 
  fread("1-Data/Archi/Development_Rep4_SMSE.csv", data.table = F, sep=';', dec=',', fill= T)%>%
  mutate(Transplanting_Date= lubridate::dmy(.data$Transplanting_Date),
         Observation_Date= lubridate::dmy(.data$Observation_Date))


# Updating development file -----------------------------------------------

# Computing the MAP -------------------------------------------------------

development= Vpalmr::compute_MAP(x = development)

# Writing the new development file with updated MAP and Transplanting_Date.
# development%>%
#   arrange(Trial, Progeny, Observation_Date, TreeNumber)%>%
#   mutate(Observation_Date= format(Observation_Date, "%d/%m/%Y"),
#          Transplanting_Date= format(Transplanting_Date, "%d/%m/%Y"))%>%
#   data.table::fwrite("1-Data/Archi/Development_Rep4_SMSE.csv", sep=";")


# Updating the parameter file for MAP: ------------------------------------

# Computing the total number of leaves emitted from planting: -------------

development2= 
  development%>% 
  group_by(.data$TreeNumber,.data$MonthAfterPlanting)%>%
  summarise(Nb_frond_new= mean(.data$Nb_frond), LeafIndexRank1= max(.data$LeafIndexRank1))%>%
  arrange(TreeNumber, MonthAfterPlanting)%>%
  mutate(Nb_frond_new= ifelse(is.na(.data$Nb_frond_new),
                              .data$LeafIndexRank1-lag(.data$LeafIndexRank1),
                              .data$Nb_frond_new),
         TotalEmitted= cumsum(.data$Nb_frond_new))%>%
  select(-.data$LeafIndexRank1)%>%
  merge(development,., by= c("TreeNumber","MonthAfterPlanting"),sort = F)%>%
  mutate(Nb_frond= .data$Nb_frond_new)%>%select(-.data$Nb_frond_new)

# Using a moving average to obtain an average total number of emitted leaves per MAP:
MAP_average=
  development2%>%
  group_by(.data$MonthAfterPlanting,.data$Progeny)%>%
  summarise(TotalEmitted= mean(.data$TotalEmitted, na.rm= T))%>%
  bind_rows(tibble::tibble(MonthAfterPlanting= 1, 
                           Progeny= unique(development2$Progeny), TotalEmitted= 1),.)

# Using a table with all possible MAPS in the date sequence, and filling it when there is data:
df_MAP=
  expand.grid(Progeny= unique(MAP_average$Progeny),
              MonthAfterPlanting= 1:max(MAP_average$MonthAfterPlanting))%>%
  left_join(MAP_average,c("MonthAfterPlanting","Progeny"))%>%
  group_by(.data$Progeny)%>%
  mutate(nbLeaves= zoo::na.approx(.data$TotalEmitted,.data$MonthAfterPlanting))%>%
  mutate(nbLeaves= round(.data$nbLeaves))%>%
  select(-.data$TotalEmitted)%>%
  rename(MAP= .data$MonthAfterPlanting)

ggplot(data = development2, aes(x = MonthAfterPlanting, y= TotalEmitted))+
  facet_wrap(.~Progeny)+
  geom_line(aes(group= TreeNumber))+
  geom_point(data= df_MAP, aes(x = MAP, y= nbLeaves, color= "Fit"))

# plot_test= 
#   ggplot(data = development2%>%filter(Progeny=="DY4"),
#          aes(x = MonthAfterPlanting, y= TotalEmitted))+
#   geom_line(aes(group= TreeNumber))
# ggplotly(plot_test)

# Recompute the total number of leaves emmitted from planting -------------

parameters2=
  right_join(parameters%>%select(-.data$nbLeaves),df_MAP, by= c("MAP","Progeny"))%>%
  mutate(Date= min(lubridate::dmy(.data$Date), na.rm = T) + months(.data$MAP))%>%
  mutate(Year= lubridate::year(.data$Date), Month= lubridate::month(.data$Date))%>%
  mutate(Date= format(.data$Date, '%d/%m/%Y'))

# data.table::fwrite(parameters2, "1-Data/Archi/ParameterSimu.csv", sep= ";")



# Updating the Area file --------------------------------------------------
area_df= data.table::fread("1-Data/Archi/LeafArea_monitoring_SMSE.csv", data.table = F, sep=';',
                           fill= T, dec=".")
is(area_df$Width)
area_df[is.na(as.numeric(area_df$Width)),]


Planting_date_area=
  area_df%>%
  select(.data$TreeNumber,.data$FieldPlantingDate)%>%
  na.omit()%>%
  group_by(.data$TreeNumber)%>%
  summarise(Transplanting_Date= unique(.data$FieldPlantingDate))

area_df= 
  area_df%>%
  dplyr::left_join(Planting_date_area, by= "TreeNumber")%>%
  dplyr::mutate(Obs_Date= lubridate::dmy(.data$Obs_Date),
                FieldPlantingDate= lubridate::dmy(.data$FieldPlantingDate))%>%
  dplyr::mutate(FieldPlantingDate= lubridate::dmy(.data$Transplanting_Date))%>%
  dplyr::mutate(MAP= lubridate::interval(.data$FieldPlantingDate,.data$Obs_Date)%/%
                  months(1))%>%
  dplyr::mutate(Obs_Date= format(.data$Obs_Date, "%d/%m/%Y"),
                FieldPlantingDate= format(.data$FieldPlantingDate, "%d/%m/%Y"))%>%
  select(-.data$Transplanting_Date)

# data.table::fwrite(area_df, "1-Data/Archi/LeafArea_monitoring_SMSE.csv", sep=";")

# Checking if the data could be imported ----------------------------------

path_data= '1-Data/Archi'
test= 
  Vpalmr::import_data(parameter= file.path(path_data,'ParameterSimu.csv'),
                      development= file.path(path_data,'Development_Rep4_SMSE.csv'),
                      phylotaxy= file.path(path_data,'Stem_SMSE14.csv'),
                      declination= file.path(path_data,'AnglesC&A_SMSE_Nov14.csv'),
                      curvature= file.path(path_data,'LeafCurvature_SMSE14.csv'),
                      leaf_area= file.path(path_data,'LeafArea_monitoring_SMSE.csv'),
                      axial_angle= file.path(path_data,'LeafDispositionComp_SMSE14.csv'),
                      petiole_width= file.path(path_data,'Petiole_SMSE14.csv'),
                      twist= file.path(path_data,'Torsion_SMSE14.csv'), map = 60)

Vpalmr::test_Area(x = test$Area, path = "C:/Users/vezy/Desktop")

Palm_Param= compute_archi(map = 60, data_path = "1-Data/Archi",
                          write_path = "../VPalm_Architecture")

Palm_Param= compute_archi(map = 60, data_path = "1-Data/Archi",
                          write_path = "../VPalm_Architecture")

Palm_Param= readRDS(file = "../VPalm_Architecture/models_MAP_60.RData")

make_scene(data = Palm_Param, nleaves = 45, Progeny = NULL, ntrees= 0, 
           path = "C:/Users/vezy/Desktop", 
           AMAPStudio = "D:/OneDrive/Travail_AMAP/PalmStudio/VPalm_IDE",
           overwrite = T)
# extract_params(data = Palm_Param$input, model = Palm_Param$model, leaves = 45,)

test_plot=
  ggplot(test$DataAll, aes(x= MonthAfterPlanting, y= PosB))+
  facet_wrap(.~Progeny)+
  geom_point(aes(group= TreeNumber))
plotly::ggplotly(test_plot)


OPFs= make_opf_all(parameter = "C:/Users/vezy/Desktop/VPalm_inputs",
                   opf = "C:/Users/vezy/Desktop/scenes/opf",
                   AMAPStudio = "vpalm.jar", overwrite = T)



out=
    make_opf(parameter = "C:/Users/vezy/Desktop/VPalm_inputs/DA1_Average_MAP_60_2.txt",
             opf = "C:/Users/vezy/Desktop/scenes/opf/DA1_Average_MAP_60_2.opf",
             AMAPStudio = "D:/OneDrive/Travail_AMAP/PalmStudio/VPalm_IDE",
             overwrite = T)


# There are too much missing FrondRank values (no values on new data). How can we compute it ? We 
# need it for the estimation of the rachis length.





# Adding tests  -----------------------------------------------------------

test2= test_pos_on_leaflet_all(test$Area)
test2

list(
  nbLeafEmitted= "Total cumulative number of emitted leaves from planting",
  rachisLengthIntercept= "Intercept of the rachis length equation (cm)",
  rachisLengthSlope= "Slope of the rachis length equation (cm.leaves-1)",
  Nbmax= "Maximum number of leaflets per leaf (-)",
  nbSlope= "Slope of the number of leaflets per leaf ~ rachis length (leaflets.cm-1)",
  nbInfl= "Inflexion point of the number of leaflets per leaf ~ rachis length (cm)",
  lenfletLengthAtBIntercept = "Intercept of the leaflet length at B point ~ rachis length (cm)",
  leafletLengthAtBSlope = "Slope of the allometry between leaflet length at B point and rachis length (cm.cm-1)",
  bWidthIntercept= "Intercept of the leaflet width at B point ~ rachis length (cm)",
  bWidthSlope= "Slope of the leaflet width at B point ~ rachis length (cm.cm-1)",
  xm_intercept= "Intercept of the relative position of maximum width on leaflet ~ position on rachis (-)",
  xm_slope= "Slope of the relative position of maximum width on leaflet ~ position on rachis (-)",
  ym_intercept= "Intercept of leaflet shape ~ position on rachis (-)",
  ym_slope= "Slope of the leaflet shape ~ position on rachis (-)",
  petioleRachisRatio_M= "Ratio of the petiole length to rachis length (cm.cm-1)",
  nbFronds_M= "Number of green leaves on the palm (leaves)",
  decMaxA= "Maximum declination angle (i.e leaf curvature) at point A (degrees)",
  decSlopeA= "Slope at inflexion point of the declination angle (i.e leaf curvature) at A point ~ at C point (degrees.degrees-1)",
  decInflA= "Inflexion point of the declination angle at A point ~ at C point (degrees)"
)
