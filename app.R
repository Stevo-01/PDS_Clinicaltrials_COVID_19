if(!require(shiny)) install.packages('shiny')
if(!require(shinythemes)) install.packages('shinythemes')

# When you read the data make sure it is in 'data' file
# just change the working dirctory and read the data you need
#example: setwd("C://Users//yousif//Desktop//PDS_Clinicaltrials_COVID_19")

# Load modules ----
source("modules/home.R")
source("modules/map.R")
source("modules/analytics.R")
source("modules/data.R")
source("modules/team_info.R")
source("modules/guide.R")


# Define UI ----
ui <- navbarPage(
    theme = "Darkly",
    inverse = TRUE,
    title = div(
        "Clinicaltrials_COVID_19"
    ),
    windowTitle = "Clinicaltrials_COVID_19",

    # HomePage ----
    tabPanel("Home", homeUI(id = "home")),

    # MapPage ----
    tabPanel("Map", mapUI(id = "map")),

    # AnalyticsPage ----
    tabPanel("Analytics", analyticsUI(id = "analytics")),

    # DataPage ----
    tabPanel("Data", dataUI(id = "data")),
    
    #More pages ----
    navbarMenu("More",
               tabPanel("User Guide", guideIU('guide')),
               tabPanel("About Us", aboutUsUI(id = "team_info"))
    )

  
)

# Define Server logic ----
server <- function(input, output, session) {
  
  homeServer('home')
  mapServer('map')
  analyticsServer('analytics')
  dataServer('data')

}

# Run the App ----
shinyApp(ui = ui, server = server)