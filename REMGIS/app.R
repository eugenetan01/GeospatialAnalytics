#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("REMGIS"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
         sidebarPanel(
            fileInput("file1", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
          ), 
         sliderInput("distance",
                     "Distance value:",
                     min = 1000,
                     max = 10000,
                     value = 2000)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        leafletOutput("distPlot")
        #tabsetPanel(
          #tabPanel("Order Locations", leafletOutput("map",width="80%",height="400px")),
          #tabPanel("Markers", verbatimTextOutput("markers"))
        #)
      )
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
  
  #Get reactive ppp object for plotting
  test <- reactive({
    req(input$file1)
    
    if (is.null(input$file1))
      return(NULL)
    
    firms <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
    firms <- subset(firms,type>="Legal")
    
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
    #x    <- faithful[, 2] 
    #distance <- seq(min(x), max(x), length.out = input$distance)
    #
  
    #temp_sgdf <- as.SpatialGridDataFrame.im(kde_sp_100)
    #proj4string(temp_sgdf) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572
                                  #+datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
    #qtm(temp_sgdf)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

