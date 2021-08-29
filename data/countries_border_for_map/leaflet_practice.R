# This file is explaining how world map was created
# Step by step

#*** Note: change the directory to the file which contain the data
setwd("C://Users//yousif//Desktop//countries_border_for_map")
#--------------------------------------------------



# Creating world map in r with help of leaflet package
# install the package first

# Loading the libraries
# Note: ignor the Warning
library(leaflet)
library(rgdal)
library(raster)
library(htmltools)
#-------------------------------
# Step1:

# Getting the shapefile for world countries border, to use it with leaflet
countries<-shapefile('C://Users//yousif//Desktop//countries_border_for_map//World_Countries__Generalized_.shp')
# vector which contain the specific countries that we want
wn<-c('United States', 'France', 'China', 'Canada','United Kingdom','Germany','Russian Federation','Malaysia')

# Getting the data border for the countries that we want
wc<-countries[countries$COUNTRY %in% wn,]
# Here is a first look at the map with higlighting only the wanted countries
leaflet()%>%
  setView(2,2,2)%>%
  addProviderTiles(providers$CartoDB.DarkMatter)%>%
  addPolygons(data=wc,
              color = "#2345FA", weight = 1, smoothFactor = 0.5)

# wait few second it will open on viewer window

#--------------------------------------------------------------
#Step2:

# Now adding our data set for medical studies depending on country.
study_data<-read.csv("C://Users//yousif//Desktop//countries_border_for_map//COVID19_studies_based_on_countries.csv")



#--------------------------------------------------------
#Step3:

# now creating Choropleth map
# first prepare the colors and bins for 'colorBin' function
hc<-c('#000A41','#001273','#001CB1')
bins<-c(6,50,100,200,516)

# next we will reorder the dataset orders to match the shapefile
std1=study_data[c(4,5,1,2,6,8,3,7),]
# now creating the colorBin funcion
pal<-colorBin(palette =hc,domain = std1$Number.of.Studies,bins = bins)


# Maping using leaflet and fillcolor function

leaflet()%>%
  setView(2,2,2)%>%
  addProviderTiles(providers$CartoDB.DarkMatter)%>%
  addPolygons(data=wc,
              weight = 1, smoothFactor = 0.5,fillOpacity = 0.8,
              fillColor = pal(std1$Number.of.Studies))

# the new map has more colors now 
# lighter the color, the more study the country has.
#--------------------------------
#Step4:

# now we will creat highlight over the countries
leaflet()%>%
  setView(2,2,2)%>%
  addProviderTiles(providers$CartoDB.DarkMatter)%>%
  addPolygons(data=wc,
              weight = 1, smoothFactor = 0.5,fillOpacity = 0.8,
              fillColor = pal(std1$Number.of.Studies),
              highlightOptions=highlightOptions(weight = 5,color = '#0028FF',
                               fillOpacity = 0.7,
                               bringToFront = TRUE
              ))

# try to move you mouse over any country in the map now.

#-----------------------
#Step5:
# creating pop-up labels

lab<-paste('<p>',std1$Country.Name,'</p>',
             '<p>',"Total studies on COVID-19:  ",std1$Number.of.Studies,'</p>',
             '<p>','Treatment Studies:',std1$Treatment.Studies,'</p>',
             '<p>','Prevention Studies:',std1$Prevention.Studies,'</p>',
             '<p>','Vaccine Studies:',std1$Vaccine.Studies,'</p>',
             '<p>','Diagnostic Studies:',std1$Diagnostic.Studies,'</p>',
             '<p>','Number of Volunteers Enrolled:',std1$Enrollment,'</p>',
             sep = ""
             )

# now adding the pop-up labels into the leaflet map
leaflet()%>%
  setView(2,2,2)%>%
  addProviderTiles(providers$CartoDB.DarkMatter)%>%
  addPolygons(data=wc,
              weight = 1, smoothFactor = 0.5,fillOpacity = 0.8,
              fillColor = pal(std1$Number.of.Studies),
              highlightOptions=highlightOptions(weight = 5,color = '#0028FF',
                                                fillOpacity = 0.7,
                                                bringToFront = TRUE
              ),label = lapply(lab, HTML))
#---------------------------
# Last step:
# adding legend to the map

leaflet()%>%
  setView(2,2,2)%>%
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
#------------------
# done have any question just ask me 



