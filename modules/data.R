# always remember to change the working directory


# Use this code to load the libraries you need
if(!require(dplyr)) install.packages('dplyr')
if(!require(shiny)) install.packages('shiny')
if(!require(DT)) install.packages('DT')
# When you read the data make sure it is in this form
# just change the working dirctory and read the data you need

# any_data <- read.csv("data/any_data.csv")
# any_data2 <- read.csv("data/any_data.csv")
#------------------------
#Example of datatable using uncleaned data

u_data<-read.csv('data/unprocessed_data/COVID_19_CCT.csv')

dataUI <- function(id) {
    ns <- NS(id)
    fluidPage(style = "background-color:#1D1F28 ;background-size: cover;",
              fluidRow(
                  br(),br(),br(),br(),br(),br(),br(),
                  aos(
                      element = 
                          div(align = "center",
                              h3(style="font-family: Arial ; font-weight: 900; color: #6467D3 ; font-size: 30px ","DATA PAGE"),
                              h1(style="font-style: Arial ;font-weight: bold; color: #FEFEFE; font-size: 20px;","data search with link to study page",),
                              #line
                              column(width = 4,offset = 4,
                                     h1(style="font-style: Arial ;font-weight: bold; font-size: 50px;border-bottom: 0.5px solid #6467D3",""))
                          ), 
                      animation = "fade-up", 
                      duration = "2000"),
                  br(),br(),br(),
                  div(align = "center",
                      dataTableOutput(ns('rowData'),width = "65%")
                  ),br(),br(),br(),br()
              ),
              
    ) 
    
}

#----------------------------(Backend code)
# Module server function
dataServer<-function(id, stringsAsFactors) {
    moduleServer(
        id,
        ## Below is the module function
        function(input, output,session) {
            output$rowData<-renderDataTable({
                
                u_data[,c(3,27)]
                
            })
            
})}