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
    print(input$year)
  })
  
  output$year = renderUI({
    year.options = c('2010','2014','2016','2018')
    
    selectInput('year', label = 'Year', choices = year.options, selected = '2018')
  })
  
  CDI_data = reactive({
    read_csv(case_when(
      input$year == 2010 ~ "CDI_2010.csv",
      input$year == 2014 ~ "CDI_2014.csv",
      input$year == 2016 ~ "CDI_2016.csv",
      input$year == 2018 ~ "CDI_2018.csv",
      #input$year == 2020 ~ "CDI_2020.csv"
    ))%>%
      unite('data_type', DataValueType:DataValueUnit, sep = ' ', remove = TRUE,na.rm = TRUE)
     
    
  })
  
  state_SVI = reactive({
    state_SVI_full%>%
      filter(year == input$year)%>%
      select(state,state_SVI)
  })
  
  output$topic = renderUI({
    topic.options = CDI_data()%>%
    {unique(.$Topic)}
    
  selectInput('topic',label = 'Chronic Disease', choices = topic.options, selected = 'Cardiovascular Disease')
  })
    
    
  output$questions = renderUI({
    
    question.options = CDI_data()%>%
      filter(Topic == input$topic)%>%
      {unique(.$Question)}
    
    
    selectInput('question', label = 'Metric', choices = question.options, selected = 'Mortality from total cardiovascular diseases')
    
  })
  
  output$datatype = renderUI({
    datatype.options = CDI_data()%>%
      filter(Topic == input$topic)%>%
      filter(Question == input$question)%>%
      {unique(.$data_type)}
    
    selectInput('datatype', label = 'Unit', choices = datatype.options, selected = 'Age-adjusted Rate cases per 100,000')
  })
  
  output$group1 = renderUI({
    group1.options = CDI_data()%>%
      filter(Topic == input$topic)%>%
      filter(Question == input$question)%>%
      filter(data_type == input$datatype)%>%
      {unique(.$Stratification1)}
    
    selectInput('group1', label = 'Group 1', choices = group1.options, selected = 'Male')
  })
  
  output$group2 = renderUI({
    group2.options = CDI_data()%>%
      filter(Topic == input$topic)%>%
      filter(Question == input$question)%>%
      filter(data_type == input$datatype)%>%
      {unique(.$Stratification1)}
    
    selectInput('group2', label = 'Group 2', choices = group2.options, selected = 'Female')
  })
  
  output$useDiffButtons = renderUI({
    radioButtons(
      inputId = 'diffChoice',
      label = "Use difference between Group1 and Group2?", 
      choices = c(
        "Use only Group1" = "noDiff", 
        "Use difference between Group 1 and Group 2" = "useDiff"))
  })
  
  df = reactive({
    if(input$diffChoice == "noDiff"){
      CDI_data()%>%
        filter(Topic == input$topic)%>%
        filter(Question == input$question)%>%
        filter(data_type == input$datatype)%>%
        filter(Stratification1 == input$group1) %>% 
        select(LocationAbbr,Stratification1,DataValue)%>%
        pivot_wider(names_from = Stratification1, values_from = DataValue)%>%
        mutate(diff = get(input$group1)) %>% 
        rename(state = LocationAbbr)%>%
        drop_na()%>%
        left_join(state_SVI()) %>% 
        drop_na()
    } else {
      CDI_data()%>%
        filter(Topic == input$topic)%>%
        filter(Question == input$question)%>%
        filter(data_type == input$datatype)%>%
        filter(Stratification1 %in% c(input$group1,input$group2)) %>% 
        select(LocationAbbr,Stratification1,DataValue)%>%
        pivot_wider(names_from = Stratification1, values_from = DataValue)%>%
        mutate(diff = get(input$group1) - get(input$group2)) %>% 
        rename(state = LocationAbbr)%>%
        drop_na()%>%
        left_join(state_SVI()) %>% 
        drop_na()
    }
    
    # CDI_data()%>%
    #   filter(Topic == input$topic)%>%
    #   filter(Question == input$question)%>%
    #   filter(data_type == input$datatype)%>%
    #   filter(
    #     if_else(condition = input$diffChoice == "noDiff", 
    #             true = Stratification1 == input$group1, 
    #             false = Stratification1 %in% c(input$group1,input$group2))) %>% 
    #   select(LocationAbbr,Stratification1,DataValue)%>%
    #   pivot_wider(names_from = Stratification1, values_from = DataValue)%>%
    #   mutate(
    #     diff = if_else(
    #       condition = input$diffChoice == "noDiff", 
    #       true = get(input$group1),
    #       false = get(input$group1) - get(input$group2))) %>% 
    #   rename(state = LocationAbbr)%>%
    #   drop_na()%>%
    #   left_join(state_SVI())
    
    # tempdf = CDI_data()%>%
    #   filter(Topic == input$topic)%>%
    #   filter(Question == input$question)%>%
    #   filter(data_type == input$datatype)
    # 
    # tempdf = tempdf %>% 
    #   if_else(condition = input$diffChoice == "noDiff", 
    #           true = filter(Stratification1 == input$group1),
    #           false = filter(Stratification1 %in% c(input$group1,input$group2)))%>%
    #   select(LocationAbbr,Stratification1,DataValue)
    # 
    # tempdf = tempdf %>% 
    #   pivot_wider(names_from = Stratification1, values_from = DataValue)
    # 
    # tempdf = tempdf %>% 
    #     if_else(condition = input$diffChoice == "noDiff",
    #             true = mutate(diff = input$group1),
    #             false = mutate(diff = get(input$group1) - get(input$group2)))%>%
    #     rename(state = LocationAbbr)%>%
    #     drop_na()%>%
    #     left_join(state_SVI())
  })
  
  output$table=renderDataTable({
    CDI_data()
  })
  
  output$svidiff = renderPlotly({
    fit = lm(data = df(), diff~state_SVI)
    #print(summary(fit))
    # test = summary(fit)
    # rp = vector('expression', 2)
    # rp[1] = substitute(expression(italic(R)^2 == MYVALUE),
    #                    list(MYVALUE = format(test$adj.r.squared, dig=3)))[2]
    # rp[2] = substitute(expression(italic(p) == MYOTHERVALUE),
    #                    list(MYOTHERVALUE = format(test$coefficients[2, 4], dig=2)))[2]
    
    if(input$diffChoice == "noDiff") {
      df()%>%
        plot_ly( x = ~state_SVI, y = ~diff, text = ~state, type = "scatter")%>%
        add_markers(y = ~diff) %>% 
        layout(title = paste(input$question, "for", input$group1, "Vs State SVI"), yaxis = list(title = input$datatype),xaxis = list(title = "Social Vulnerability Index per State")) %>% 
        add_lines(x = ~state_SVI, y = fitted(fit))
    }else{
      df()%>%
      plot_ly( x = ~state_SVI, y = ~diff, text = ~state, type = "scatter")%>%
      add_markers(y = ~diff) %>% 
      layout(title = paste("The Difference Between", input$group1 ,"and", input$group2, "on", input$question, "Vs State SVI"), yaxis = list(title = input$datatype),xaxis = list("Social Vulnerability Index per State")) %>% 
      add_lines(x = ~state_SVI, y = fitted(fit)) #%>% 
      # legend('topright', legend = rp, bty = 'n')
    }
    
    
      
  })
  
  output$svifit = renderPrint({
    fit = lm(data = df(), diff~state_SVI)
    summary(fit)
  })
  
  output$diffmap = renderPlotly({
    plot_diff(df(),~state,~diff,input$year,input$question,input$datatype)
  })
  
  
  output$heatmap = renderPlotly({
    plot_heatmap(CDI_data(),state_SVI(),input$topic,input$question,input$datatype)
  })
  


})
