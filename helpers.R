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
# crimes <- read.csv("C:/Users/W447075/Documents/ST558/Comora_final/hate_crimes.csv")
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

#create subsets of the data based on region
crimes_ne <- crimes %>% filter(region == "northeast")
crimes_west <- crimes %>% filter(region == "west")
crimes_south <- crimes %>% filter(region == "south")
crimes_midwest<- crimes %>% filter(region == "midwest")

#create dataframe for unsupervised learning
df <- column_to_rownames(crimes, var = "state")
df <- na.omit(df)
df <- select(df, -12)
df_scale <- scale(df)