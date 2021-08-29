# always remember to change the working directory


# Use this code to load the libraries you need
if(!require(dplyr)) install.packages('dplyr')
if(!require(shiny)) install.packages('shiny')
if(!require(shinymaterial)) install.packages('shinymaterial')
if(!require(shinyWidgets)) install.packages('shinyWidgets')
if(!require(shinydashboard)) install.packages('shinydashboard')
if(!require(aos)) install.packages('aos')

# When you read the data make sure it is in 'data' file
# just change the working dirctory and read the data you need
# example: setwd("C://Users//yousif//Desktop//PDS_Clinicaltrials_COVID_19")
#-------

# any_data <- read.csv("data/any_data.csv")
# any_data2 <- read.csv("data/any_data.csv")

homeUI <- function(id) {
  #ns <- NS(id)
  fluidPage(
    use_aos(),
    # Top of the page
    fluidRow(style = "background-image: url(https://www.tulsa-health.org/sites/default/files/styles/fhd/public/2020-09/GettyImages-1215392772.jpg?itok=aOUQtP4J); background-size: cover;background-attachment: fixed;background-position: center;background-repeat: no-repeat;",
             column(width = 5,offset =1 ,
             div(aos(
               element = 
                             div(br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                 h3(style="font-family: Arial ; font-weight: 900; color: #5C6098; font-size: 30px ","PRINCIPLE OF DATA SCIENCE"),
                                 h1(style="font-style: Arial ;font-weight: bold; color: #FEFEFE; font-size: 50px ","Shinny APP: Analysis of",br(), "COVID 19 Clinical Trial",
                                    br(),"Studies Globally")), 
               animation = "fade-up", 
               duration = "2000"),
             )),
             column(width = 12,offset = 3,style = "height:200px;",
                    div(align = "center",
             aos(
               element = img(style = "height:100px;",src="https://www.um.edu.my/images/img-logo-UM.png"), 
               animation = "fade-up", 
               duration = "2000")
             ))
      ),
    # second part of the page
    fluidRow(style = "background-color: #6467D3;",

                   # Title
                   div(align = "center",
                      
                           aos(
                             element = h3(style="font-family: Arial, Helvetica, sans-serif ;font-weight: bold; color:#000000; font-size: 30px ",br(),br(),br(),br(),"WHAT IS",br(),br()), 
                             animation = "fade-up", 
                             duration = "2000"),
                           
                           aos(
                             element = column(width = 6,offset=3,style = "font-style: --; font-weight: bold;color:#FFFFFF; font-size: 60px;border-bottom: 0.5px solid white",
                                          "COVID-19 Clinical Trial Studies", br(),br()), 
                             animation = "fade-up", 
                             duration = "1500")
                       
                   ),
                   # Text Defintion
                   
                       column(width=6,offset=3,div(
                           aos(
                             element = p(style="font-family: Arial; color: #000000; font-size: 29px;text-align: justify; ", br(),br(),"They are research studies performed in people that are aimed at evaluating a medical, surgical, or behavioral intervention. They are the primary way that researchers find out if a new treatment, like a new drug or diet or medical device is safe and effective againist COVID-19", br(),br(),br(),br(),), 
                             animation = "fade-up", 
                             duration = "2000")
                       ))
                   
      ),br(),br(),
    #-
    # Explaining page---1
    fluidRow(
      div(align="center",
          aos(
            element = div(
                          
                              h3(style="font-family: -- ;font-style: --; color: #6467D3; font-size: 30px;font-weight: 900; ","App Objective"),
                              column(width=6,offset=3,h1(style="font-family: -- ;font-style: --;font-weight: bold; color: #000000; font-size: 32px;text-align: center; ","The App aims to Analyse a data collection of COVID-19 medical studies using Data Science tools.",br(),br()))
                          
            ),
            animation = "fade-up",
            duration = "1500"
          )
          )
      
    ),br(),br(),
    #-----
    #Explaining Data row
    fluidRow(
      column(width = 4,offset = 2,
             aos(
               element =div(h3(img(src="images/g-d-i.png",style="width: 50px"),style="color: #6467D3; font-weight: bold;","Data Collection"),
                              p(style="color: #000000;text-align: justify;font-size: 25px;","The data represent a collection of studies obtain from 'clinicaltrials.gov', which is a registry of clinical trials. Each row in the data represents a medical study related to COVID-19 and its corresponding attributes, such as type, Enrollment, completion, and so on.")
                            ),
               animation = "fade-up",
               duration = "1500"
             )
             ),
      column(width = 4,offset = 1,
             aos(
               element =div(h3(img(src="images/p-d-i.png",style="width: 50px"),style="color: #6467D3; font-weight: bold;","Data Preprocessing"),
                            p(style="color: #000000;text-align: justify;font-size: 25px;","The data has 4174 rows, where each row represents a study and 27 columns representing attributes corresponding to each study. After the pre-processing and removing missing values and useless columns, the data became 2052 rows and 12 columns.")
               ),
               animation = "fade-up",
               duration = "1500"
             )
             ),
    ),br(),br(),
    #-------
    fluidRow(
      column(width = 4,offset = 2,
             aos(
               element =div(h3(img(src="images/p-i.png",style="width: 50px"),style="color: #6467D3; font-weight: bold;","Phase of the Study"),
                            p(style="color: #000000;text-align: justify;font-size: 25px;","Clinical trial studies have 4 phases that represent the progress of the study."),
                            img(src="images/p-e.jpg")
               ),
               animation = "fade-up",
               duration = "1500"
             )
      ),
      column(width = 4,offset = 1,
             aos(
               element =div(h3(img(src="images/t-i.png",style="width: 50px"),style="color: #6467D3; font-weight: bold;","Type of the Study"),
                            p(style="color: #000000;text-align: justify;font-size: 25px;","Each medical study can be categorized into a specific category depending on the objective of the study. there are many types, but the app focuses on Treatment, Prevention, Test, and Vaccine, which is a part of Prevention studies.")
               ),
               animation = "fade-up",
               duration = "1500"
             )
      ),
    ),br(),br(),
    #-----------
    #Analytic
    fluidRow(
      column(width = 4,offset = 2,
             aos(
               element =div(h3(img(src="images/a-i.jpg",style="width: 50px"),style="color: #6467D3; font-weight: bold;","Data Analysis"),
                            p(style="color: #000000;text-align: justify;font-size: 25px;","The App focuses on Descriptive and Diagnostic Analysis. the analysis has been done with the help of R and tidyverse."),
               ),
               animation = "fade-up",
               duration = "1500"
             )
      ),
      column(width = 4,offset = 1,
             aos(
               element =div(h3(img(src="images/o-i.jpg",style="width: 50px"),style="color: #6467D3; font-weight: bold;","Outcome of The App"),
                            p(style="color: #000000;text-align: justify;font-size: 25px;","The App provides two products:",br(),"World Map: The map shows country contribution in medical studies against COVID-19",br(),"Graphs: A graphical representation of the finding from the descriptive and diagnostic analysis.")
               ),
               animation = "fade-up",
               duration = "1500"
             )
      ),br(),br(),br(),br(),
    )
    
      
    )

     
    
    
  
}



homeServer <- function(input, output, session) {

} 


# style references
#style = "background-image: url(); background-size: cover;"
#style = "height:1000px;"
#style="font-family: -- ;font-style: --; color: --; font-size: --px ",
shinyApp(ui = homeUI, server = homeServer)

