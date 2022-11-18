library(shiny)
library(tidyverse)
library(shinythemes)


county_SVI = read_csv('SVI2018_US_COUNTY.csv')

CDI_data = read_csv('CDI_2018.csv')%>%
  unite('data_type', DataValueType:DataValueUnit, sep = ' ', remove = TRUE,na.rm = TRUE)  

state_SVI = county_SVI%>%
  group_by(ST_ABBR)%>%
  filter(SPL_THEMES>0)%>%
  mutate(popxsvi = E_TOTPOP*SPL_THEMES)%>%
  summarise(state_totpop = sum(E_TOTPOP),state_SVI = sum(popxsvi)/state_totpop)%>%
  mutate(state = ST_ABBR)%>%
  select(state,state_SVI)

topic.options = unique(CDI_data$Topic)