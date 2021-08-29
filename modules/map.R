# always remember to change the working directory


# Use this code to load the libraries you need
if(!require(dplyr)) install.packages('dplyr')
if(!require(shiny)) install.packages('shiny')
if(!require(shinymaterial)) install.packages('shinymaterial')
if(!require(aos)) install.packages('aos')
if(!require(shinydashboard)) install.packages('shinydashboard')
if(!require(leaflet)) install.packages('leaflet')
if(!require(rgdal)) install.packages('rgdal')
if(!require(raster)) install.packages('raster')
if(!require(htmltools)) install.packages('htmltools')
# When you read the data make sure it is in 'data' file
# just change the working dirctory and read the data you need
#example: setwd("C://Users//yousif//Desktop//PDS_Clinicaltrials_COVID_19")

# any_data <- read.csv("data/any_data.csv")
# any_data2 <- read.csv("data/any_data.csv")


#----------
# down here is just a made up example for the map 
countries<-shapefile('data//countries_border_for_map//World_Countries__Generalized_.shp')
wn<-c('United States', 'France', 'China', 'Canada','United Kingdom','Germany','Russian Federation','Malaysia')
wc<-countries[countries$COUNTRY %in% wn,]
study_data<-read.csv("data//countries_border_for_map//COVID19_studies_based_on_countries.csv")
hc<-c('#000A41','#001273','#001CB1')
bins<-c(6,50,100,200,516)
std1=study_data[c(4,5,1,2,6,8,3,7),]
pal<-colorBin(palette =hc,domain = std1$Number.of.Studies,bins = bins)
lab<-paste('<p>',std1$Country.Name,'</p>',
           '<p>',"Total studies on COVID-19:  ",std1$Number.of.Studies,'</p>',
           '<p>','Treatment Studies:',std1$Treatment.Studies,'</p>',
           '<p>','Prevention Studies:',std1$Prevention.Studies,'</p>',
           '<p>','Vaccine Studies:',std1$Vaccine.Studies,'</p>',
           '<p>','Diagnostic Studies:',std1$Diagnostic.Studies,'</p>',
           '<p>','Number of Volunteers Enrolled:',std1$Enrollment,'</p>',
           sep = ""
)


#-----------------



mapUI <- function(id) {
  ns <- NS(id)
    fluidPage(style = "background-color:#1D1F28 ;background-size: cover;",
      #map section
      fluidRow(
               br(),br(),br(),br(),br(),br(),br(),
               aos(
                 element = 
                   div(align = "center",
                       h3(style="font-family: Arial ; font-weight: 900; color: #6467D3 ; font-size: 30px ","INTERACTIVE WORLD MAP"),
                       h1(style="font-style: Arial ;font-weight: bold; color: #FEFEFE; font-size: 50px;","Mapping COVID-19 Medical Studies",),
                       #line
                       column(width = 4,offset = 4,
                       h1(style="font-style: Arial ;font-weight: bold; font-size: 50px;border-bottom: 0.5px solid #6467D3",""))
                       ), 
                 animation = "fade-up", 
                 duration = "2000"),
               br(),br(),br(),
               div(align = "center",
                   leafletOutput(ns('myMap'),width = "65%",height = "450px")
               ),br(),br(),br(),br(),br(),br(),br(),
               aos(
                 element = 
                   div(align = "center",
                       h3(style="font-family: Arial ; font-weight: 900; color: #6467D3 ; font-size: 30px ","Here we can add control of the different maps"),
                       h1(style="font-style: Arial ;font-weight: bold; color: #FEFEFE; font-size: 50px;","I really hate this map now ruin all my code, also this is just example map to check if it works",),
                       ), 
                 animation = "fade-up", 
                 duration = "2000"),br(),br(),br(),br()
    )
    )
}




## Server Module
mapServer<-function(id, stringsAsFactors) {
  moduleServer(
    id,
  function(input, output, session) {
  
  output$myMap<-renderLeaflet({
    leaflet()%>%setMaxBounds(-180, -90, 180, 90) %>%
      setView(0, 20, zoom = 2)%>%
      addProviderTiles(providers$CartoDB.DarkMatter)%>%
      addPolygons(data=wc,
                  weight = 1, smoothFactor = 0.5,fillOpacity = 0.8,
                  fillColor = pal(std1$Number.of.Studies),
                  highlightOptions=highlightOptions(weight = 5,color = '#0028FF',
                                                    fillOpacity = 0.7,
                                                    bringToFront = TRUE
                  ),label = lapply(lab, HTML))%>%
      addLegend(
        pal = pal,
        values = std1$Number.of.Studies,
        opacity = 0.7,
        position = "bottomright"
      )
  })
 
})}

