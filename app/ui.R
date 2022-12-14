#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




shinyUI(fluidPage(
    
    theme = shinytheme("simplex"),
  
    # Application title
    titlePanel("2018 Chronic Diseases in the US and Social Vulnerability Index Correlation"),
    
    fluidRow(
      column(3, uiOutput('year')),
      column(3, uiOutput('topic')),
      column(3, uiOutput('questions')),
      column(3, uiOutput('datatype'))
      
    ),
    

    
    fluidRow(
      column(4, uiOutput('useDiffButtons')),
      column(4, uiOutput('group1')),
      column(4, uiOutput('group2'))
    ),
    
    
    tabsetPanel(type = 'tabs',
      tabPanel("Map",
        fluidRow(
         plotlyOutput('diffmap')
       ),
      ),

      
      tabPanel("SVI and Disparity",
        fluidRow(
          plotlyOutput('svidiff'),
          verbatimTextOutput("svifit")
        ),
      ),
      tabPanel("Heatmap",
      fluidRow(
        plotlyOutput('heatmap')
        ),
      ),      
      tabPanel("Table",
        fluidRow(
         dataTableOutput('table')
        ),
      ),      
      
    ),
    
    fluidRow(
      textOutput('debug')
    )
))
