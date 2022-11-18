#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    theme = shinytheme("simplex"),
  
    # Application title
    titlePanel("2018 Chronic Diseases in the US and Social Vulnerability Index Correlation"),
    
    fluidRow(
      column(4, selectInput('topic',label = 'Chronic Disease', choices = topic.options, selected = 'Cardiovascular Disease')),
      column(4, uiOutput('questions')),
      column(4, uiOutput('datatype'))
    ),
    
    fluidRow(
      column(6, uiOutput('group1')),
      column(6, uiOutput('group2'))
    ),
    
    
    tabsetPanel(type = 'tabs',
      
      tabPanel("Table",
        fluidRow(
         dataTableOutput('table')
        ),
      ),
      
      tabPanel("SVI and Disparity",
        fluidRow(
          
        ),
      ),
      
    ),
    
    fluidRow(
      textOutput('debug')
    )
))