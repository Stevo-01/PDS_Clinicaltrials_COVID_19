# always remember to change the working directory


# Use this code to load the libraries you need
if(!require(dplyr)) install.packages('dplyr')
if(!require(shiny)) install.packages('shiny')
if(!require(shinymaterial)) install.packages('shinymaterial')
if(!require(aos)) install.packages('aos')
if(!require(shinydashboard)) install.packages('shinydashboard')
if(!require(ggplot2)) install.packages('ggplot2')
# When you read the data make sure it is in this form
# just change the working dirctory and read the data you need

# any_data <- read.csv("data/any_data.csv")
# any_data2 <- read.csv("data/any_data.csv")

#---------
# graph example dataframe:



#----------(Frontend code)
# User interface
analyticsUI <- function(id) {
  ns <- NS(id)
  fluidPage(style = "background-color:#1D1F28 ;background-size: cover;",
    fluidRow(
      br(),br(),br(),br(),br(),br(),br(),
      aos(
        element = 
          div(align = "center",
              h3(style="font-family: Arial ; font-weight: 900; color: #6467D3 ; font-size: 30px ","ANALYTIC PAGE"),
              h1(style="font-style: Arial ;font-weight: bold; color: #FEFEFE; font-size: 20px;","here we have to state the qustions about the data and answer them using graphs and plots",),
              #line
              column(width = 4,offset = 4,
                     h1(style="font-style: Arial ;font-weight: bold; font-size: 50px;border-bottom: 0.5px solid #6467D3",""))
          ), 
        animation = "fade-up", 
        duration = "2000"),
      br(),br(),br(),
      div(align = "center",
          plotOutput(ns('example'),width = "65%",height = "450px")
      ),br(),br(),br(),br()
    ),
    
    )
  
  
 
}

#----------------------------(Backend code)
# Module server function
analyticsServer<-function(id, stringsAsFactors) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output,session) {
  dfe <- data.frame(
    group = c("Male", "Female", "Child"),
    value = c(25, 25, 50))
  output$example<-renderPlot({
    # Example dataframe
    # Example plot
    ggplot(dfe, aes(x="", y=value, fill=group))+
      geom_bar(width = 1, stat = "identity")

 })
  
})}