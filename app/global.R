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

plot_heatmap = function(cdi, svi, topic, question,datatype){
  df = cdi%>%
    mutate(state = LocationAbbr)%>%
    filter(Topic == topic)%>%
    filter(Question == question)%>%
    filter(data_type == datatype)%>%
    select(state,Stratification1,DataValue,data_type)
  

  
  states = unique(df$state)
  groups = unique(df$Stratification1)
  diff_arr = list()
  
  for (i in groups){
    g1 = df%>%
      filter(Stratification1 == i)
    
    for (j in groups){
      
      
      g2 = df%>%
        filter(Stratification1 == j)%>%
        left_join(g1,"state")%>%
        mutate(diff = DataValue.x - DataValue.y)%>%
        left_join(svi,"state")%>%
        select(Stratification1.x,Stratification1.y,diff,state_SVI)
      
      
      
      #print(g1)
      #print(g2)
      
      slope = summary(lm(diff~state_SVI,g2))$coefficients[2]
      rsqadj = summary(lm(diff~state_SVI,g2))$adj.r.squared
      
      diff_arr = append(diff_arr,list(c(i,j,slope,rsqadj)))
      
      
    }
    
  }
  heatmap_df = as.data.frame(do.call(rbind,diff_arr))%>%
    rename(c("group1" = V1,
             "group2" = V2,
             "slope" = V3,
             "rsqadj" = V4))%>%
    mutate(slope = as.numeric(slope))%>%
    mutate(rsqadj = as.numeric(rsqadj))
  heatmap_df$rsqadj[is.nan(heatmap_df$rsqadj)]<-0
  p <-ggplot(heatmap_df,aes(x=group1,y=group2,fill = slope,size = rsqadj))+
    geom_tile()+
    scale_fill_distiller(palette = "RdBu")
  
  return(ggplotly(p))
}