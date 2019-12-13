#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

shinyServer(function(input, output,session){
  
  crimes <- read.csv("st558projectfinal/hate_crimes.csv")
  # initialize variables for each region of the U.S. 
  northeast <- c("Maine","New Hampshire","Vermont",
                 "Massachusetts","Rhode Island",
                 "Connecticut","New York","New Jersey",
                 "Pennsylvania")
  midwest <- c("Ohio","Michigan","Indiana","Illinois",
               "Wisconsin","Minnesota","Iowa","Missouri",
               "North Dakota", "South Dakota","Nebraska",
               "Kansas")
  south <- c("Texas","Oklahoma","Arkansas","Louisiana",
             "Mississippi","Tennesee","Alabama","Georgia",
             "Florida","West Virginia", "Kentucky",
             "Delaware","Maryland","Virginia","North Carolina",
             "South Carolina","District of Columbia")
  west <- c("Washington","Idaho","Montana","Wyoming",
            "Oregon","California","Nevada","Utah",
            "Arizona","Colorado","New Mexico",
            "Alaska","Hawaii")
  region <- vector()
  for (i in 1:length(crimes$state)){
    if(is.element(crimes$state[i], northeast)==TRUE){
      region[i]= "northeast"}
    else if(is.element(crimes$state[i], midwest)==TRUE){
      region[i]= "midwest"}
    else if(is.element(crimes$state[i], west)==TRUE){
      region[i] = "west"}
    else {region[i] = "south"}
  }   
  
  #define new column in data set with region assigned to state
  crimes$region <- region
  #create dataframe for unsupervised learning
  df <- column_to_rownames(crimes, var = "state")
  df <- na.omit(df)
  df <- select(df, -12)
  df_scale <- scale(df)
     output$introduction <- renderText({
        "This is my introduction"
    })
     
     output$data <- renderText({
       "This page allows you to create some basic graphical
       and numeric summaries including a scatter plot, boxplot,
       and statistical summary for any of the variables in the 
       'crimes' dataset."
     })
    
    selectedData <- reactive({
      crimes %>% filter(region ==input$region)
    })
    selectData2 <- reactive({
      crimes[ , c("region",input$y)]
    })
    
    selectData3 <- reactive({
      crimes[ , c("region", input$y2)]
    })
    
    mydata <- reactive({
      crimes[ ,input$var]
    })
    
    mycluster <- reactive({
      kmeans(df_scale, centers = input$k, nstart = 25)
    })
    
    d <- reactive({
      dist(df, method = input$dmethod)
    })
     
    clusterplot <- reactive({
      plot(hclust(d(), method = input$cmethod))
    })
    
    modelmethod <- reactive({
      input$regmethod
    })
    
    p <- reactive({
      ggplot(crimes, aes_string(x = input$x,
                                y = input$y)) + 
        geom_point(size = 3) + 
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.y = element_blank())
    })
    
    output$save <- downloadHandler(
      filename = "save.png",
      content = function(file){
        ggsave(p(), filename = file)
      })
      
    dt <- reactive({
      input$mytable1
    })
    
    output$table <- downloadHandler(
      filename = "table.csv",
      content = function(file){
        s = input$mytable1_rows_all
        write.csv(df[s, ,drop = FALSE],file,row.names = TRUE)
      }
    )
    
    #render scatter plot
    output$scatterplot <- renderPlot({
      scatter <- ggplot(crimes, aes_string(x = input$x,
                                        y = input$y)) + 
        geom_point(size = 3) + 
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.y = element_blank())
      scatter
    })
    
    output$hover_info <- renderPrint({
      cat("input$plot_hover:\n")
      str(input$plot_hover$coords_img)
    })
    
    output$boxplot <- renderPlot({
      ggplot(selectData3(), aes(x = region, y = selectData3()[ ,input$y2])) + 
        geom_boxplot(aes(fill = region)) + ylab(input$y2)+
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.y = element_blank())
    })
    
    #render numerical summary
    output$summary <- renderPrint({
      dataset <- na.omit(mydata())
      summary(dataset)
    })
    
    output$cluster <- renderPlot({
      fviz_cluster(mycluster(),data = df)
    })
    
    output$tree <- renderPlot({
      clusterplot()
    })
    
    #render data table with ability to filter
    output$mytable1 <- DT::renderDataTable({
      DT::datatable(df,class = "display nowrap compact",
                    filter = "top")
    })
    
    output$model <- renderPrint({
      myformula <- as.formula(paste(input$yvar,"~",input$xvar))
      model <- train(myformula, df,
                     method = modelmethod(),
                     trControl = trainControl(
                       method = "cv", number = 10,
                       verboseIter = TRUE))
      print(model)
    })

})

#run the application
shinyApp(ui,server)


