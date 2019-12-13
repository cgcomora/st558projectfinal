#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Variables Impacting Hate Crime Rates"),

    # Sidebar with a slider input for number of bins
    # define main panel layout
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Information",
                                 textOutput("introduction")),
                        tabPanel("Data Exploration",
                                 downloadButton("save", "Save scatter plot"),
                                 textOutput("data"),
                                 column(6,selectInput("x","Explanatory variable:",
                                             choices = colnames(crimes),
                                             selected = "median_household_income")),
                                 column(6,selectInput("y","Response variable:",
                                             selected = "median_household_income",
                                            choices = colnames(crimes))),
                                 fluidRow(
                                     column(6,plotOutput("scatterplot",height = 250,
                                            width = 250,
                                            hover = hoverOpts(id = "plot_hover"))),
                                     column(6,verbatimTextOutput("hover_info"))),
                                 selectInput("y2", "Variable to compare:",
                                                       selected = "median_household_income",
                                                       choices = colnames(crimes)),
                                 plotOutput("boxplot"),
                                 fluidRow(
                                 selectInput("var","Summary variable:",
                                             choices = colnames(crimes),
                                             selected = "median_household_income"),
                                 verbatimTextOutput("summary")))
                        ,
                        tabPanel("Unsupervised Learning",
                                 selectInput("k","Number of clusters:",
                                             choices = c(1,2,3),
                                             selected = 1),
                                 plotOutput("cluster"),
                                 selectInput("dmethod","Distance method:",
                                             choices = c("euclidian","binary","minkowski",
                                                         "canberra","manhattan","maximum"),
                                             selected = "euclidian"),
                                 selectInput("cmethod", "Cluster method:",
                                             choices = c("single","complete"),
                                             selected = "single"),
                                 plotOutput("tree")),
                        tabPanel("Modeling",
                                 selectInput("xvar", "x variable:",
                                                choices = colnames(crimes),
                                                selected = "gini_index"),
                                 selectInput("yvar", "y variable:",
                                                choices = colnames(crimes),
                                                selected = "median_household_income"),
                                 selectInput("regmethod", "Modeling method:", 
                                             choices = c("glm","rf"),
                                             selected = "glm"),
                                 verbatimTextOutput("model")),
                        tabPanel("Data",
                                 downloadButton("table","Download data"),
                                 DT::dataTableOutput("mytable1"))
        )
    )
))
