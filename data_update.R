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

# Updating parameter file -------------------------------------------------
parameters= fread("1-Data/Archi/ParameterSimu.csv", data.table = F)

development= 
  fread("1-Data/Archi/Development_Rep4_SMSE.csv", data.table = F, sep=';', dec=',', fill= T)%>%
  mutate(Transplanting_Date= lubridate::dmy(.data$Transplanting_Date),
         Observation_Date= lubridate::dmy(.data$Observation_Date))


# Computing the MAP -------------------------------------------------------

Planting_date_df= 
  development%>%
  select(.data$TreeNumber,.data$Transplanting_Date)%>%
  na.omit()%>%
  group_by(.data$TreeNumber)%>%
  summarise(Transplanting_Date= unique(.data$Transplanting_Date))

# Re-computing the MAP:
development=
  dplyr::left_join(development%>%select(-Transplanting_Date),Planting_date_df)%>%
  dplyr::mutate(MAP_comp= lubridate::interval(Transplanting_Date,Observation_Date) %/% months(1))%>%
  group_by(TreeNumber)%>%
  arrange(Observation_Date)%>%
  dplyr::mutate(MAP_comp= ifelse(!is.na(Nb_frond)&!is.na(lag(Nb_frond))&Nb_frond==lag(Nb_frond)&
                                   !is.na(LeafIndexRank1)&!is.na(lag(LeafIndexRank1))&
                                   LeafIndexRank1==lag(LeafIndexRank1),
                                 lag(MAP_comp),MAP_comp))%>%
  dplyr::mutate(MAP_comp= ifelse(!is.na(Nb_frond)&!is.na(lead(Nb_frond))&Nb_frond==lead(Nb_frond)&
                                   !is.na(LeafIndexRank1)&!is.na(lead(LeafIndexRank1))&
                                   LeafIndexRank1==lead(LeafIndexRank1),
                                 lead(MAP_comp),MAP_comp))%>%ungroup()
# NB: The last two mutates are used for the case when one session is made on several different days


# Ask Doni about this particular entry:
# SMSE	1	B66	4	DY4	105_28	BN13	DY	07/12/2009	22/12/2010	47	08/12/2014	23				1																			
# It is very weird, and if we remove it we feel that there is a missing campaign
# I think it is maybe a problem of date: MAP 30	and obs_date 27/06/2013 should be better
# development$MAP_comp[development$MAP_comp_test]= 
#   development$MAP_comp[which(development$MAP_comp_test)-1]

# Also, ask Doni about these two lines:
# SMSE	1	B66	4	DY4	104_31						24/07/2018			96
# SMSE	1	B66	4	DY4 	104_31						13/11/2018			74
# The last one should have a higher LeafIndexRank1 but has a lower one. It makes the TotalEmitted go lower.

# Data.frame with error between "measured" and re-computed MAP: 
MAP_error_df= 
  development%>%
  select(TreeNumber, Observation_Date,Transplanting_Date,
         MonthAfterPlanting,MAP_comp)%>%
  mutate(MAP_error= MonthAfterPlanting-MAP_comp)%>%
  na.omit()%>%
  filter(MAP_error!=0)
# If any error is found, please check the values and replace them if necessary 

MAP_err_plot= 
  development%>%
  ggplot(aes(x= MonthAfterPlanting, y= MAP_comp, color= TreeNumber))+
  geom_point()+
  geom_abline(intercept= 0, slope= 1)
# ggplotly(MAP_err_plot)

# Carefull ! Execute the following line of code only if the MAP is not correct: 
development$MonthAfterPlanting= development$MAP_comp
# development= development%>%select(-.data$MAP_comp)


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

# development2%>%filter(TreeNumber=="107_29")%>%
#   select(Observation_Date,TotalEmitted,MonthAfterPlanting,MAP_comp,Nb_frond,LeafIndexRank1)

# Using a moving average to obtain an average total number of emitted leaves per MAP:
MAP_average=
  development2%>%
  group_by(MonthAfterPlanting,Progeny)%>%
  summarise(TotalEmitted= mean(TotalEmitted, na.rm= T))%>%
  bind_rows(tibble::tibble(MonthAfterPlanting= 1, 
                           Progeny= unique(development2$Progeny), TotalEmitted= 1),.)

df_MAP=
  expand.grid(Progeny= unique(MAP_average$Progeny),
              MonthAfterPlanting= 1:max(MAP_average$MonthAfterPlanting))%>%
  left_join(MAP_average,c("MonthAfterPlanting","Progeny"))%>%
  group_by(Progeny)%>%
  mutate(TotalEmitted= zoo::na.approx(TotalEmitted,MonthAfterPlanting))%>%
  mutate(TotalEmitted= round(TotalEmitted))

ggplot(data = development2, aes(x = MonthAfterPlanting, y= TotalEmitted))+
  facet_wrap(.~Progeny)+
  geom_line(aes(group= TreeNumber))+
  geom_point(data= df_MAP, aes(color= "Fit"))

test= 
  ggplot(data = development2%>%filter(Progeny=="DY4"),
         aes(x = MonthAfterPlanting, y= TotalEmitted))+
  geom_line(aes(group= TreeNumber))
ggplotly(test)


# Recompute the total number of leaves emmitted from planting -------------

parameters2=
  left_join(parameters,models)%>%
  mutate(nbLeaves_2= round(slope*MAP))

View(parameters2)
plot(parameters2$nbLeaves,parameters2$nbLeaves_2)
