#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

shinyServer(function(input, output, session){
  
  library(shiny)
  library(factoextra)
  library(plyr)
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(tibble)
  library(tree)
  library(DiagrammeR)
  library(rpart)
  library(caret)
  library(plotly)
  
  
  # read in the data
  crimes <- read.csv("hate_crimes.csv")
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
  
  #assign region based on state
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
  
  #render text for introduction
       output$introduction1 <- renderText({
        "The purpose of this ShinyApp is to allow users to explore data on
         hate crimes from the website www.fivethirtyeight.com.  The data contains
         information on hate crimes reported to the Southern Poverty Law Center (SPLC) 
         and the FBI.  For the purposes of this app, we do not focus on how the data was
         collected.  Our interests are solely in exploring the data and using 
         supervised learning methods (generalized linear models and random forests)
         to determine which variables in the data set are the best predictor of
         hatecrimes, as measured by the SPLC or FBI."  
    })
       output$introduction2 <- renderText({
         "The app allows you to use a scatter plot to visually explore the relationships between 11 different
         variables including median household income, share of the population with a high
         school degree, and the number of hate crimes per 100k people.  The data represents
         45 states (several have been removed due to missing values).  I have also added
         a categorical variable called 'region' to allow you to compare variables across
         each with a boxplot.  On the tab labeled 'Unsupervised Learning' you are able to 
         conduct a Cluster Analysis, varying the number of clusters, to find subgroups of observations 
         within the data and visualize the hierarchical relationships with a dendrogram.  Finally, 
         you can explore some models using two supervised learning methods and make some predictions.  
         There is also a 'Data' tab that allows you to scroll through the data, filter and download
         to a csv file.  Please note that for the Clustering Analysis you must specify the
         method for computing the distance between each pair of obervations."
       })
     
     output$data <- renderText({
       "This page allows you to create some basic graphical
       and numeric summaries including a scatter plot, boxplot,
       and statistical summary for any of the variables in the 
       'crimes' dataset."
     })
    
    #render reactive variable for box plot data
    selectData3<- reactive({
      crimes[ , c("region", input$y2)]
    })
    
    #render reactive variable for numerical summary
    mydata <- reactive({
      crimes[ ,input$var]
    })
    
    #kmeans cluster analysis
    mycluster <- reactive({
      kmeans(df_scale, centers = input$k, nstart = 25)
    })
    
    #distance calc for dendrogram
    d <- reactive({
      dist(df, method = input$dmethod)
    })
     
    #cluster plot
    clusterplot <- reactive({
      plot(hclust(d(), method = input$cmethod))
    })
    
    #render reactive variable for modelling method
    modelmethod <- reactive({
      input$regmethod
    })
    
    #render 
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
    output$scatterplot <- renderUI({
               scatter <- ggplot(crimes, aes_string(x = input$x,
                                             y = input$y)) + 
          geom_point(size = 3,aes_string(colour = "red")) + 
          theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.text.y = element_blank())
        scatter})
     
  
    
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
      summary(model)
      })
    
    #output$predict <- renderPrint({
     # predict(model, newdata = data.frame(c(value())))
  #  })

})




