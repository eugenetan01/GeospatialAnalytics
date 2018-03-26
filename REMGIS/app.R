#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Interactive link view 
library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(
   div(class="outer",
       tags$head(
         includeCSS("styles.css")
       ),
       leafletOutput("distPlot"),
       absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                   draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                   width = 330, height = "auto",
                   
                   h2("REMGIS"),
     
            fileInput("file1", "Choose CSV File",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
            ), 
            selectInput(inputId="sector",
                        label="Sector:",
                        choices=c("Legal" = "Legal",
                                  "Banking" = "Bank",
                                  "Consultancy" = "Consultancy",
                                  "Accounting" = "Accountacy",
                                  "Architectural" = "Architectural"),
                        selected = "Legal"),
            sliderInput("distance",
                       "Distance value:",
                       min = 1000,
                       max = 10000,
                       value = 1300)
          )
      
      # Show a plot of the generated distribution
      #mainPanel(
        #leafletOutput("distPlot")
        #tabsetPanel(
          #tabPanel("Order Locations", leafletOutput("map",width="80%",height="400px")),
          #tabPanel("Markers", verbatimTextOutput("markers"))
        #)
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  packages = c('ggmap', 'ggplot2', 'tmap', 'sf', 'rgdal', 'maptools', 'spatstat')
  for(p in packages){
    if(!require(p, character.only = T)){
      install.packages(p)
    }
    library(p,character.only = T)
  }
  
  library(ggmap)
  library(ggplot2)
  library(tmap)
  library(sf)
  library(shiny)
  library(leaflet)
  
  #upload file reuse
  getFile <- reactive({
    req(input$file1)
    
    if (is.null(input$file1))
      return(NULL)
    
    firms <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
    return(firms)
  })
  
  getInputSector <- reactive({
      sector <- input$sector
      return(sector)
  })
  
  #Get reactive ppp object for plotting
  test <- reactive({
    firms <- getFile()
    firms <- subset(firms,type>=getInputSector())
    
    firms_shp <- st_as_sf(firms, coords = c("lon", "lat"), crs = 4326)
    firms_shp<-st_transform(firms_shp, 3414)
    firms_sp1 = as(firms_shp, 'Spatial')
    firms_sp2 = as(firms_sp1, 'SpatialPoints')
    
    subzones <- readOGR(dsn="shp/master-plan-2014-subzone-boundary-no-sea", layer="MP14_SUBZONE_NO_SEA_PL")
    subzone_cr <- subzones[subzones$REGION_C == "CR",]
    cr_sp = as(subzone_cr, 'SpatialPolygons')
    
    cr_owin <- as.owin.SpatialPolygons(cr_sp)
    
    point.ppp <- ppp(coordinates(firms_sp2)[,1], coordinates(firms_sp2)[,2], window = cr_owin)
    
    return(point.ppp)
    #make into WGS84
    #return the spTransform 
  })
  
  #Plot the ppp object based on slider input on tmap
  output$distPlot <- renderLeaflet({
    
    library(tmap)
    library(tmaptools)
    library(raster)
    kde_sp_100 <- density(test(), input$distance)
    
    r<-raster(kde_sp_100)
    crs(r) <- CRS("+init=epsg:3414")
    leaflet() %>%
      addTiles()%>%
      addRasterImage(r, colors = "Spectral", opacity = 0.4)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

