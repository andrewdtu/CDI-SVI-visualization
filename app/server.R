#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#





shinyServer(function(input, output) {
  
  output$debug = renderText({
    print('text')
  })
  
  output$questions = renderUI({
    
    question.options = CDI_data%>%
      filter(Topic == input$topic)%>%
      {unique(.$Question)}
    
    
    selectInput('question', label = 'Metric', choices = question.options, selected = 'Mortality from total cardiovascular diseases')
    
  })
  
  output$datatype = renderUI({
    datatype.options = CDI_data%>%
      filter(Topic == input$topic)%>%
      filter(Question == input$question)%>%
      {unique(.$data_type)}
    
    selectInput('datatype', label = 'Unit', choices = datatype.options, selected = 'Age-adjusted Rate cases per 100,000')
  })
  
  output$group1 = renderUI({
    group1.options = CDI_data%>%
      filter(Topic == input$topic)%>%
      filter(Question == input$question)%>%
      filter(data_type == input$datatype)%>%
      {unique(.$Stratification1)}
    
    selectInput('group1', label = 'Group 1', choices = group1.options, selected = 'Male')
  })
  
  output$group2 = renderUI({
    group2.options = CDI_data%>%
      filter(Topic == input$topic)%>%
      filter(Question == input$question)%>%
      filter(data_type == input$datatype)%>%
      {unique(.$Stratification1)}
    
    selectInput('group2', label = 'Group 2', choices = group2.options, selected = 'Female')
  })
  
  df = reactive({
    CDI_data%>%
      filter(Topic == input$topic)%>%
      filter(Question == input$question)%>%
      filter(data_type == input$datatype)%>%
      filter(Stratification1 %in% c(input$group1,input$group2))%>%
      select(LocationAbbr,Stratification1,DataValue)%>%
      pivot_wider(names_from = Stratification1, values_from = DataValue)%>%
      mutate(diff = get(input$group1) - get(input$group2))%>%
      rename(state = LocationAbbr)%>%
      drop_na()%>%
      left_join(state_SVI)
  })
  
  output$table=renderDataTable({
    df()
  })
  
  output$svidiff = renderPlotly({
    fit = lm(data = df(), diff~state_SVI)
    df()%>%
      plot_ly( x = ~state_SVI, y = ~diff, text = ~state, type = "scatter")%>%
      add_markers(y = ~diff) %>% 
      add_lines(x = ~state_SVI, y = fitted(fit))
      
  })
  
  output$diffmap = renderPlotly({
    plot_diff(df(),~state,~diff)
  })
  


})
