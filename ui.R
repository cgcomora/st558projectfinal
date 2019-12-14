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
                                 br(),
                                 tags$b("This is my first ShinyApp!"),
                                 br(),
                                 tags$hr(),
                                 tags$a(href = "https://fivethirtyeight.com/features/higher-rates-of-hate-crimes-are-tied-to-income-inequality/","Click here for my data source!"),
                                 br(),
                                 br(),
                                 textOutput("introduction1"),
                                 br(),
                                 withMathJax(helpText('General Linear Model Form:
                                                      $$Y = \\beta_0\\ + \\beta_1X$$')),
                                 br(),
                                 textOutput("introduction2")),
                        tabPanel("Data Exploration",
                                 br(),
                                 downloadButton("save", "Save scatter plot"),
                                 br(),
                                 br(),
                                 textOutput("data"),
                                 br(),
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
                                     column(6,verbatimTextOutput("hover_info"),
                                            radioButtons("color","Color",
                                                         c("red","blue")))),
                                 br(),
                                 br(),
                                 selectInput("y2", "Variable to compare with boxplot:",
                                                       selected = "median_household_income",
                                                       choices = colnames(crimes)),
                                 plotOutput("boxplot"),
                                 fluidRow(
                                 selectInput("var","Summary variable:",
                                             choices = colnames(crimes),
                                             selected = "median_household_income"),
                                 verbatimTextOutput("summary"))),
                        tabPanel("Unsupervised Learning",
                                 br(),
                                 br(),
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
                                 br(),
                                 br(),
                                 selectInput("xvar", "x variable:",
                                                choices = colnames(crimes),
                                                selected = "gini_index"),
                                 selectInput("yvar", "y variable:",
                                                choices = colnames(crimes),
                                                selected = "median_household_income"),
                                 selectInput("regmethod", "Modeling method:", 
                                             choices = c("glm","rf"),
                                             selected = "glm"),
                                 verbatimTextOutput("model"),
                                 numericInput("value", "Value of Predictor Var:",0.1,min =0.1,max = 1.0),
                                 verbatimTextOutput("predict")),
                        tabPanel("Data",
                                 br(),
                                 br(),
                                 downloadButton("table","Download data"),
                                 br(),
                                 br(),
                                 DT::dataTableOutput("mytable1"))
    )
)))
