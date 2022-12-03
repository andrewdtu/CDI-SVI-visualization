library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)
library(plotly)

# county_SVI = read_csv('SVI2018_US_COUNTY.csv')



# state_SVI = county_SVI%>%
#   group_by(ST_ABBR)%>%
#   filter(SPL_THEMES>0)%>%
#   mutate(popxsvi = E_TOTPOP*SPL_THEMES)%>%
#   summarise(state_totpop = sum(E_TOTPOP),state_SVI = sum(popxsvi)/state_totpop)%>%
#   mutate(state = ST_ABBR)%>%
#   select(state,state_SVI)
state_SVI_full = read_csv('SVI_full.csv')



plot_diff <- function(data,location,value){
  plot_geo(data,locationmode='USA-states')%>%
    add_trace(locations = location,z = value, color = value, colors = 'Oranges')%>%
    layout(
      title = 'Title WIP: make it some combination of the options',
      geo = c(scope = 'usa',projection = list(type = 'albers usa'))
    )
}