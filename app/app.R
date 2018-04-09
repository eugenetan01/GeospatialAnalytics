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
  mainPanel(
    tabsetPanel(
      tabPanel("Kernel Density Interactive Map",
        div(class="outer",
           tags$head(
             includeCSS("styles.css")
           ),
           leafletOutput("distPlot", width="100%",height="1000px"),
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
                                      "Accounting" = "Accountancy",
                                      "Architectural" = "Architectural"),
                            selected = "Legal"),
                sliderInput("distance",
                           "Distance value:",
                           min = 100,
                           max = 1000,
                           value = 280)
              )
        )
      ),
      tabPanel("Kernel Density Map Comparison",
               div(class="outer",
                   tags$head(
                     includeCSS("styles.css")
                   )
               ),
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                             width = 330, height = "auto",
                             h2("REMGIS"),  
                             checkboxGroupInput("industries", "Show",
                                                choices = c(
                                                  "Legal" = "Legal",
                                                  "Banking" = "Bank",
                                                  "Consultancy" = "Consultancy",
                                                  "Accounting" = "Accountancy",
                                                  "Architectural" = "Architectural"
                                                )
                             ),
                             sliderInput("distance1",
                                         "Distance value:",
                                         min = 100,
                                         max = 1000,
                                         value = 280)
               ),
               fluidRow(
                 conditionalPanel(condition="input.industries.indexOf('Legal') > -1",
                 column(width = 6, class = "well",
                        h4("Legal kernel Density"),
                        leafletOutput("plotLegal")
                 )), 
                 conditionalPanel(condition="input.industries.indexOf('Bank') > -1",
                 column(width = 6, class="well",
                        h4("Banks Kernel Density"),
                        leafletOutput("plotBank")
                 )),  
                 conditionalPanel(condition="input.industries.indexOf('Consultancy') > -1",
                 column(width = 6, class="well",
                        h4("Consultancy Kernal Density"),
                        leafletOutput("plotConsultancy")
                 )),    
                 conditionalPanel(condition="input.industries.indexOf('Accountancy') > -1",
                 column(width = 6, class="well",
                        h4("Accountancy Kernel Density"),
                        leafletOutput("plotAccountancy")
                 ))
               ,
                 conditionalPanel(condition="input.industries.indexOf('Architectural') > -1",
                 column(width = 6, class="well",
                        h4("Architectural Kernal Density"),
                        leafletOutput("plotArchitectural")
                 ))      
               )
      ),
      tabPanel("Location Quotient",
               div(class="outer",
                   tags$head(
                     includeCSS("styles.css")
                   )),
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                             width = 330, height = "auto",
                             h2("REMGIS")
                             ,checkboxGroupInput("lq_industry", "Show",
                                                 choices = c(
                                                   "Legal" = "Legal",
                                                   "Banking" = "Bank",
                                                   "Consultancy" = "Consultancy",
                                                   "Accounting" = "Accountancy",
                                                   "Architectural" = "Architectural"
                                                 )
                             )
               ),fluidRow(
                 column(width = 12, class="well",
                        uiOutput("analysis")
                 ),
                 conditionalPanel(condition="input.lq_industry.indexOf('Legal') > -1",
                                  column(width = 12, class = "well",
                                         h4("Legal Firms Location Quotient Analysis"),
                                         uiOutput("lqLegal")
                                  )),
                 conditionalPanel(condition="input.lq_industry.indexOf('Bank') > -1",
                                  column(width = 12, class = "well",
                                         h4("Banking Firms Location Quotient Analysis"),
                                         uiOutput("lqBank")
                                  )),
                 conditionalPanel(condition="input.lq_industry.indexOf('Consultancy') > -1",
                                  column(width = 12, class = "well",
                                         h4("Consultancy Firms Location Quotient Analysis"),
                                         uiOutput("lqConsultancy")
                                  )),
                 conditionalPanel(condition="input.lq_industry.indexOf('Accountancy') > -1",
                                  column(width = 12, class = "well",
                                         h4("Accountancy Firms Location Quotient Analysis"),
                                         uiOutput("lqAccountancy")
                                  )),
                 conditionalPanel(condition="input.lq_industry.indexOf('Architectural') > -1",
                                  column(width = 12, class = "well",
                                         h4("Architectural Firms Location Quotient Analysis"),
                                         uiOutput("lqArchitectural")
                                  )),
                 column(width = 12, class="well",
                        plotOutput("LQPlot")
                 ),
                 column(width = 12, class="well",
                        plotOutput("LQPlotJurong")
                 )
               )
      )         
    )   
  )
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
   
  packages = c('leaflet.minicharts', 'manipulateWidget', 'ggmap', 'ggplot2', 'tmap', 'sf', 'rgdal', 'maptools', 'spatstat', 'amap', 'REAT', 'dplyr', 'gridExtra')
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
  library(leaflet.minicharts)
  library(manipulateWidget)

  #observe and update the slider input from comparison panel to KDE map panel
  observe({
    c_num <- input$distance1
    updateNumericInput(session, "distance", value = c_num)
    updateSliderInput(session, "distance",
                      value = c_num)
  })
  
  #observe and update the slider input from KDE Map panel to Comparison panel
  observe({
    c_numCompare <- input$distance
    updateNumericInput(session, "distance1", value = c_numCompare)
    updateSliderInput(session, "distance1",
                      value = c_numCompare)
  })
  
  #upload file reuse
  getFile <- reactive({
    req(input$file1)
    
    if (is.null(input$file1))
      return(NULL)
    
    firms <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
    return(firms)
  })
  
  getDataUploadedOrNot <- reactive({
    if(is.null(input$file1)) return(NULL)
    return(input$file1)
  })
  
  output$fileUploaded <- reactive({
    return(!is.null(getDataUploadedOrNot()))
  })

  outputOptions(output, "fileUploaded", suspendWhenHidden=FALSE)
  
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
  })
  
  #Get ppp object for interactive map
  getDensity <- reactive({
    kde_sp_100 <- density(test(), input$distance)
    return(kde_sp_100)
  })
  
  #Plot KDE leaflet in interactive map view 
  output$distPlot <- renderLeaflet({
    
    library(tmap)
    library(tmaptools)
    library(raster)
    kde_sp_100 <- getDensity()
    
    r<-raster(kde_sp_100)
    crs(r) <- CRS("+init=epsg:3414")
    
    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r),
                        na.color = "transparent")
    # 
    # kde_adj <- setValues(r, getValues(r))
    # 
    # summarykdeppp.values <- quantile(na.omit(getValues(kde_adj)), seq(0,1,0.2))
    # at <- c(summarykdeppp.values[1], summarykdeppp.values[2], summarykdeppp.values[3], summarykdeppp.values[4], summarykdeppp.values[5], summarykdeppp.values[6])
    # cb <- colorBin(palette = "YlOrRd", bins = at, domain = at, na.color = "#00000000", reverse=FALSE)
    # 
    leaflet() %>%
      addTiles()%>%
      addRasterImage(r, colors = "Spectral", opacity = 0.4)#%>%
      #addLegend(pal = cb, values = at, title = "Density Function", position='bottomleft', labFormat = labelFormat(digits=8),layerId="leg")
  })
  
  #Plot KDE for comparison panel
  output$plotLegal <- renderLeaflet({
    firms <- getFile()
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
    
    Legal <- density(point.ppp, input$distance)
    
    r1<-raster(Legal)
    crs(r1) <- CRS("+init=epsg:3414")
    
    leaflet() %>%
      addTiles()%>%
      addRasterImage(r1, colors = "Spectral", opacity = 0.4)%>%
      syncWith("maps")
  })
  
  #Plot KDE Leaflet for banks in comparison view 
  output$plotBank <- renderLeaflet({
    firmsBank <- getFile()
    firmsBank <- subset(firmsBank,type>="Bank")
    
    firms_shpBank <- st_as_sf(firmsBank, coords = c("lon", "lat"), crs = 4326)
    firms_shpBank<-st_transform(firms_shpBank, 3414)
    firms_sp1Bank = as(firms_shpBank, 'Spatial')
    firms_sp2Bank = as(firms_sp1Bank, 'SpatialPoints')
    
    subzonesB <- readOGR(dsn="shp/master-plan-2014-subzone-boundary-no-sea", layer="MP14_SUBZONE_NO_SEA_PL")
    subzone_crB <- subzonesB[subzonesB$REGION_C == "CR",]
    cr_spB = as(subzone_crB, 'SpatialPolygons')
    
    cr_owinB <- as.owin.SpatialPolygons(cr_spB)
    
    point.pppB <- ppp(coordinates(firms_sp2Bank)[,1], coordinates(firms_sp2Bank)[,2], window = cr_owinB)
    
    Banks <- density(point.pppB, input$distance)
    
    r2<-raster(Banks)
    crs(r2) <- CRS("+init=epsg:3414")
    
    leaflet() %>%
      addTiles()%>%
      addRasterImage(r2, colors = "Spectral", opacity = 0.4)%>%
      syncWith("maps")
    
  })
  
  #Plot KDE map for consultancy in KDE comparison view 
  output$plotConsultancy <- renderLeaflet({
    firms <- getFile()
    firms <- subset(firms,type>="Consultancy")
    
    firms_shp <- st_as_sf(firms, coords = c("lon", "lat"), crs = 4326)
    firms_shp<-st_transform(firms_shp, 3414)
    firms_sp1 = as(firms_shp, 'Spatial')
    firms_sp2 = as(firms_sp1, 'SpatialPoints')
    
    subzones <- readOGR(dsn="shp/master-plan-2014-subzone-boundary-no-sea", layer="MP14_SUBZONE_NO_SEA_PL")
    subzone_cr <- subzones[subzones$REGION_C == "CR",]
    cr_sp = as(subzone_cr, 'SpatialPolygons')
    
    cr_owin <- as.owin.SpatialPolygons(cr_sp)
    
    point.ppp <- ppp(coordinates(firms_sp2)[,1], coordinates(firms_sp2)[,2], window = cr_owin)
    
    Consultancy <- density(point.ppp, input$distance)
    
    r3<-raster(Consultancy)
    crs(r3) <- CRS("+init=epsg:3414")
    
    leaflet() %>%
      addTiles()%>%
      addRasterImage(r3, colors = "Spectral", opacity = 0.4)%>%
      syncWith("maps")
  })
  
  output$plotAccountancy <- renderLeaflet({
    # generate bins based on input$bins from ui.R
    firms <- getFile()
    firms <- subset(firms,type>="Accountancy")
    
    firms_shp <- st_as_sf(firms, coords = c("lon", "lat"), crs = 4326)
    firms_shp<-st_transform(firms_shp, 3414)
    firms_sp1 = as(firms_shp, 'Spatial')
    firms_sp2 = as(firms_sp1, 'SpatialPoints')
    
    subzones <- readOGR(dsn="shp/master-plan-2014-subzone-boundary-no-sea", layer="MP14_SUBZONE_NO_SEA_PL")
    subzone_cr <- subzones[subzones$REGION_C == "CR",]
    cr_sp = as(subzone_cr, 'SpatialPolygons')
    
    cr_owin <- as.owin.SpatialPolygons(cr_sp)
    
    point.ppp <- ppp(coordinates(firms_sp2)[,1], coordinates(firms_sp2)[,2], window = cr_owin)
    
    Accountancy <- density(point.ppp, input$distance)
    
    r4<-raster(Accountancy)
    crs(r4) <- CRS("+init=epsg:3414")
    
    leaflet() %>%
      addTiles()%>%
      addRasterImage(r4, colors = "Spectral", opacity = 0.4)%>%
      syncWith("maps")
  })
  
  #Plot KDE Comparison view for architecture firms 
  output$plotArchitectural <- renderLeaflet({
    firms <- getFile()
    firms <- subset(firms,type>="Architectural")
    
    firms_shp <- st_as_sf(firms, coords = c("lon", "lat"), crs = 4326)
    firms_shp<-st_transform(firms_shp, 3414)
    firms_sp1 = as(firms_shp, 'Spatial')
    firms_sp2 = as(firms_sp1, 'SpatialPoints')
    
    subzones <- readOGR(dsn="shp/master-plan-2014-subzone-boundary-no-sea", layer="MP14_SUBZONE_NO_SEA_PL")
    subzone_cr <- subzones[subzones$REGION_C == "CR",]
    cr_sp = as(subzone_cr, 'SpatialPolygons')
    
    cr_owin <- as.owin.SpatialPolygons(cr_sp)
    
    point.ppp <- ppp(coordinates(firms_sp2)[,1], coordinates(firms_sp2)[,2], window = cr_owin)
    
    Architectural <- density(point.ppp, input$distance)
    
    r5<-raster(Architectural)
    crs(r2) <- CRS("+init=epsg:3414")
    
    leaflet() %>%
      addTiles()%>%
      addRasterImage(r5, colors = "Spectral", opacity = 0.4)%>%
      syncWith("maps")
  })

  getAllCompaniesCBD <- reactive({
    firms <- getFile()
    firms <- firms[!duplicated(firms[,c("postal_code","type")]),]
    all_cbd <- firms[grep("Singapore 01|Singapore 02|Singapore 03|Singapore 04|Singapore 05|Singapore 06|Singapore 07|Singapore 08|Singapore 14|Singapore 15|Singapore 16|Singapore 09|Singapore 10|Singapore 11|Singapore 120|Singapore 13|Singapore 17|Singapore 18|Singapore 19|Singapore 20|Singapore 21|Singapore 22|Singapore 23|Singapore 24|Singapore 25|Singapore 26|Singapore 27|Singapore 28|Singapore 29|Singapore 30|Singapore 31|Singapore 32|Singapore 33|Singapore 34|Singapore 35|Singapore 36|Singapore 37|Singapore 38|Singapore 39|Singapore 40|Singapore 41|Singapore 42|Singapore 43|Singapore 44|Singapore 45|Singapore 57|Singapore 58|Singapore 59|Singapore 77", firms$postal_code), ]
    all_cbd_count <- nrow(all_cbd)
    return((all_cbd_count))
  })
  
  getAllCompaniesJurong <- reactive({
    firms <- getFile()
    firms <- firms[!duplicated(firms[,c("postal_code","type")]),]
    all_jur <- firms[grep("Singapore 608|Singapore 609|Singapore 6001|Singapore 6002", firms$postal_code), ]
    all_jur_count <- nrow(all_jur)
    return(all_jur_count)
  })
  
  getAllCompanies <- reactive({
    firms <- getFile()
    firms <- firms[!duplicated(firms[,c("postal_code","type")]),]
    all_country_count <- nrow(firms)
    return((all_country_count))
  })
  
  getLegalLQCBD <- reactive({
    firms <- getFile()
    firms_legal <- firms[grep("Legal", firms$type), ]
    firms_legal <- firms_legal[!duplicated(firms_legal$postal_code),]
    firms <- firms[!duplicated(firms[,c("postal_code","type")]),]
    legal_cbd <- firms_legal[grep("Singapore 01|Singapore 02|Singapore 03|Singapore 04|Singapore 05|Singapore 06|Singapore 07|Singapore 08|Singapore 14|Singapore 15|Singapore 16|Singapore 09|Singapore 10|Singapore 11|Singapore 120|Singapore 13|Singapore 17|Singapore 18|Singapore 19|Singapore 20|Singapore 21|Singapore 22|Singapore 23|Singapore 24|Singapore 25|Singapore 26|Singapore 27|Singapore 28|Singapore 29|Singapore 30|Singapore 31|Singapore 32|Singapore 33|Singapore 34|Singapore 35|Singapore 36|Singapore 37|Singapore 38|Singapore 39|Singapore 40|Singapore 41|Singapore 42|Singapore 43|Singapore 44|Singapore 45|Singapore 57|Singapore 58|Singapore 59|Singapore 77", firms_legal$postal_code), ]
    legal_cbd_count <- nrow(legal_cbd)
    all_cbd_count <- getAllCompaniesCBD()
    legal_country_count <- nrow(firms_legal)
    all_country_count <- getAllCompanies()
    var <- locq(legal_cbd_count, all_cbd_count, legal_country_count, all_country_count)
    return(var)
  })
  
  getLegalLQJur <- reactive({
    firms <- getFile()
    firms_legal <- firms[grep("Legal", firms$type), ]
    firms_legal <- firms_legal[!duplicated(firms_legal$postal_code),]
    firms <- firms[!duplicated(firms[,c("postal_code","type")]),]
    legal_jur <- firms_legal[grep("Singapore 608|Singapore 609|Singapore 6001|Singapore 6002", firms_legal$postal_code), ]
    legal_jur_count <- nrow(legal_jur)
    all_jur_count <- getAllCompaniesJurong()
    legal_country_count <- nrow(firms_legal)
    all_country_count <- getAllCompanies()
    var1 <- locq(legal_jur_count, all_jur_count, legal_country_count, all_country_count)
    return(var1)
  })
  
  getLegalCount <- reactive({
    firms <- getFile()
    firms_legal <- firms[grep("Legal", firms$type), ]
    firms_legal <- firms_legal[!duplicated(firms_legal$postal_code),]
    firms <- firms[!duplicated(firms[,c("postal_code","type")]),]
    legal_cbd <- firms_legal[grep("Singapore 01|Singapore 02|Singapore 03|Singapore 04|Singapore 05|Singapore 06|Singapore 07|Singapore 08|Singapore 14|Singapore 15|Singapore 16|Singapore 09|Singapore 10|Singapore 11|Singapore 120|Singapore 13|Singapore 17|Singapore 18|Singapore 19|Singapore 20|Singapore 21|Singapore 22|Singapore 23|Singapore 24|Singapore 25|Singapore 26|Singapore 27|Singapore 28|Singapore 29|Singapore 30|Singapore 31|Singapore 32|Singapore 33|Singapore 34|Singapore 35|Singapore 36|Singapore 37|Singapore 38|Singapore 39|Singapore 40|Singapore 41|Singapore 42|Singapore 43|Singapore 44|Singapore 45|Singapore 57|Singapore 58|Singapore 59|Singapore 77", firms_legal$postal_code), ]
    legal_cbd_count <- nrow(legal_cbd)
    return(legal_cbd_count)
  })
  
  getLegalCountJur <- reactive({
    firms <- getFile()
    firms_legal <- firms[grep("Legal", firms$type), ]
    firms_legal <- firms_legal[!duplicated(firms_legal$postal_code),]
    firms <- firms[!duplicated(firms[,c("postal_code","type")]),]
    legal_jur <- firms_legal[grep("Singapore 608|Singapore 609|Singapore 6001|Singapore 6002", firms_legal$postal_code), ]
    legal_jur_count <- nrow(legal_jur)
    return(legal_jur_count)
  })
  
  getBanksCount <- reactive({
    firms <- getFile()
    firms_bank <- firms[grep("Bank", firms$type), ]
    #firms_bank <- firms_bank[!duplicated(firms_bank$postal_code),]
    firms <- firms[!duplicated(firms[,c("postal_code","type")]),]
    bank_cbd <- firms_bank[grep("Singapore 01|Singapore 02|Singapore 03|Singapore 04|Singapore 05|Singapore 06|Singapore 07|Singapore 08|Singapore 14|Singapore 15|Singapore 16|Singapore 09|Singapore 10|Singapore 11|Singapore 120|Singapore 13|Singapore 17|Singapore 18|Singapore 19|Singapore 20|Singapore 21|Singapore 22|Singapore 23|Singapore 24|Singapore 25|Singapore 26|Singapore 27|Singapore 28|Singapore 29|Singapore 30|Singapore 31|Singapore 32|Singapore 33|Singapore 34|Singapore 35|Singapore 36|Singapore 37|Singapore 38|Singapore 39|Singapore 40|Singapore 41|Singapore 42|Singapore 43|Singapore 44|Singapore 45|Singapore 57|Singapore 58|Singapore 59|Singapore 77", firms_bank$postal_code), ]
    bank_cbd_count <- nrow(bank_cbd)
    return(bank_cbd_count)
  })
  
  getBanksCountJur <- reactive({
    firms <- getFile()
    firms_bank <- firms[grep("Bank", firms$type), ]
    #firms_bank <- firms_bank[!duplicated(firms_bank$postal_code),]
    firms <- firms[!duplicated(firms[,c("postal_code","type")]),]
    bank_jur <- firms_bank[grep("Singapore 608|Singapore 609|Singapore 6001|Singapore 6002", firms_bank$postal_code), ]
    bank_jur_count <- nrow(bank_jur)
    return(bank_jur_count)
  })
  
  getConsultancyCount <- reactive({
    firms <- getFile()
    firms_consultancy <- firms[grep("Consultancy", firms$type), ]
    firms_consultancy <- firms_consultancy[!duplicated(firms_consultancy$postal_code),]
    consultancy_cbd <- firms_consultancy[grep("Singapore 01|Singapore 02|Singapore 03|Singapore 04|Singapore 05|Singapore 06|Singapore 07|Singapore 08|Singapore 14|Singapore 15|Singapore 16|Singapore 09|Singapore 10|Singapore 11|Singapore 120|Singapore 13|Singapore 17|Singapore 18|Singapore 19|Singapore 20|Singapore 21|Singapore 22|Singapore 23|Singapore 24|Singapore 25|Singapore 26|Singapore 27|Singapore 28|Singapore 29|Singapore 30|Singapore 31|Singapore 32|Singapore 33|Singapore 34|Singapore 35|Singapore 36|Singapore 37|Singapore 38|Singapore 39|Singapore 40|Singapore 41|Singapore 42|Singapore 43|Singapore 44|Singapore 45|Singapore 57|Singapore 58|Singapore 59|Singapore 77", firms_consultancy$postal_code), ]
    consultancy_cbd_count <- nrow(consultancy_cbd)
    return(consultancy_cbd_count)
  })  
  
  getConsultancyCountJur <- reactive({
    firms <- getFile()
    firms_consultancy <- firms[grep("Consultancy", firms$type), ]
    firms_consultancy <- firms_consultancy[!duplicated(firms_consultancy$postal_code),]
    consultancy_jur <- firms_consultancy[grep("Singapore 608|Singapore 609|Singapore 6001|Singapore 6002", firms_consultancy$postal_code), ]
    consultancy_jur_count <- nrow(consultancy_jur)
    return(consultancy_jur_count)
  })  
  
  getAccountancyCount <- reactive({
    firms <- getFile()
    firms_accountancy <- firms[grep("Accountancy", firms$type), ]
    firms_accountancy <- firms_accountancy[!duplicated(firms_accountancy$postal_code),]
    accountancy_cbd <- firms_accountancy[grep("Singapore 01|Singapore 02|Singapore 03|Singapore 04|Singapore 05|Singapore 06|Singapore 07|Singapore 08|Singapore 14|Singapore 15|Singapore 16|Singapore 09|Singapore 10|Singapore 11|Singapore 120|Singapore 13|Singapore 17|Singapore 18|Singapore 19|Singapore 20|Singapore 21|Singapore 22|Singapore 23|Singapore 24|Singapore 25|Singapore 26|Singapore 27|Singapore 28|Singapore 29|Singapore 30|Singapore 31|Singapore 32|Singapore 33|Singapore 34|Singapore 35|Singapore 36|Singapore 37|Singapore 38|Singapore 39|Singapore 40|Singapore 41|Singapore 42|Singapore 43|Singapore 44|Singapore 45|Singapore 57|Singapore 58|Singapore 59|Singapore 77", firms_accountancy$postal_code), ]
    accountancy_cbd_count <- nrow(accountancy_cbd)
    return(accountancy_cbd_count)
  })  
  
  getAccountancyCountJur <- reactive({
    firms <- getFile()
    firms_accountancy <- firms[grep("Accountancy", firms$type), ]
    firms_accountancy <- firms_accountancy[!duplicated(firms_accountancy$postal_code),]
    accountancy_jur <- firms_accountancy[grep("Singapore 608|Singapore 609|Singapore 6001|Singapore 6002", firms_accountancy$postal_code), ]
    accountancy_jur_count <- nrow(accountancy_jur)
    return(accountancy_jur_count)
  })  
  
  getArchitecturalCount <- reactive({
    firms <- getFile()
    firms_architectural <- firms[grep("Architectural", firms$type), ]
    firms_architectural <- firms_architectural[!duplicated(firms_architectural$postal_code),]
    architectural_cbd <- firms_architectural[grep("Singapore 01|Singapore 02|Singapore 03|Singapore 04|Singapore 05|Singapore 06|Singapore 07|Singapore 08|Singapore 14|Singapore 15|Singapore 16|Singapore 09|Singapore 10|Singapore 11|Singapore 120|Singapore 13|Singapore 17|Singapore 18|Singapore 19|Singapore 20|Singapore 21|Singapore 22|Singapore 23|Singapore 24|Singapore 25|Singapore 26|Singapore 27|Singapore 28|Singapore 29|Singapore 30|Singapore 31|Singapore 32|Singapore 33|Singapore 34|Singapore 35|Singapore 36|Singapore 37|Singapore 38|Singapore 39|Singapore 40|Singapore 41|Singapore 42|Singapore 43|Singapore 44|Singapore 45|Singapore 57|Singapore 58|Singapore 59|Singapore 77", firms_architectural$postal_code), ]
    architectural_cbd_count <- nrow(architectural_cbd)
    return(architectural_cbd_count)
  })  
  
  getArchitecturalCountJur <- reactive({
    firms <- getFile()
    firms_architectural <- firms[grep("Architectural", firms$type), ]
    firms_architectural <- firms_architectural[!duplicated(firms_architectural$postal_code),]
    architectural_jur <- firms_architectural[grep("Singapore 608|Singapore 609|Singapore 6001|Singapore 6002", firms_architectural$postal_code), ]
    architectural_jur_count <- nrow(architectural_jur)
    return(architectural_jur_count)
  })
  
  getBankLQCBD <- reactive({
    firms <- getFile()
    firms_bank <- firms[grep("Bank", firms$type), ]
    firms_bank <- firms_bank[!duplicated(firms_bank$postal_code),]
    firms <- firms[!duplicated(firms[,c("postal_code","type")]),]
    bank_cbd <- firms_bank[grep("Singapore 01|Singapore 02|Singapore 03|Singapore 04|Singapore 05|Singapore 06|Singapore 07|Singapore 08|Singapore 14|Singapore 15|Singapore 16|Singapore 09|Singapore 10|Singapore 11|Singapore 120|Singapore 13|Singapore 17|Singapore 18|Singapore 19|Singapore 20|Singapore 21|Singapore 22|Singapore 23|Singapore 24|Singapore 25|Singapore 26|Singapore 27|Singapore 28|Singapore 29|Singapore 30|Singapore 31|Singapore 32|Singapore 33|Singapore 34|Singapore 35|Singapore 36|Singapore 37|Singapore 38|Singapore 39|Singapore 40|Singapore 41|Singapore 42|Singapore 43|Singapore 44|Singapore 45|Singapore 57|Singapore 58|Singapore 59|Singapore 77", firms_bank$postal_code), ]
    bank_cbd_count <- nrow(bank_cbd)
    bank_country_count <- nrow(firms_bank)
    all_cbd_count <- getAllCompaniesCBD()
    all_country_count <- getAllCompanies()
    return(locq(bank_cbd_count, all_cbd_count, bank_country_count, all_country_count))
  })
  
  getBankLQJur <- reactive({
    firms <- getFile()
    firms_bank <- firms[grep("Bank", firms$type), ]
    firms_bank <- firms_bank[!duplicated(firms_bank$postal_code),]
    firms <- firms[!duplicated(firms[,c("postal_code","type")]),]
    bank_jur <- firms_bank[grep("Singapore 608|Singapore 609|Singapore 6001|Singapore 6002", firms_bank$postal_code), ]
    bank_jur_count <- nrow(bank_jur)
    all_jur_count <- getAllCompaniesJurong()
    bank_country_count <- nrow(firms_bank)
    all_country_count <- getAllCompanies()
    return(locq(bank_jur_count, all_jur_count, bank_country_count, all_country_count))
  })
  
  getConsultancyLQCBD <- reactive({
    firms <- getFile()
    firms_consultancy <- firms[grep("Consultancy", firms$type), ]
    firms_consultancy <- firms_consultancy[!duplicated(firms_consultancy$postal_code),]
    consultancy_cbd <- firms_consultancy[grep("Singapore 01|Singapore 02|Singapore 03|Singapore 04|Singapore 05|Singapore 06|Singapore 07|Singapore 08|Singapore 14|Singapore 15|Singapore 16|Singapore 09|Singapore 10|Singapore 11|Singapore 120|Singapore 13|Singapore 17|Singapore 18|Singapore 19|Singapore 20|Singapore 21|Singapore 22|Singapore 23|Singapore 24|Singapore 25|Singapore 26|Singapore 27|Singapore 28|Singapore 29|Singapore 30|Singapore 31|Singapore 32|Singapore 33|Singapore 34|Singapore 35|Singapore 36|Singapore 37|Singapore 38|Singapore 39|Singapore 40|Singapore 41|Singapore 42|Singapore 43|Singapore 44|Singapore 45|Singapore 57|Singapore 58|Singapore 59|Singapore 77", firms_consultancy$postal_code), ]
    consultancy_cbd_count <- nrow(consultancy_cbd)
    consultancy_country_count <- nrow(firms_consultancy)
    all_cbd_count <- getAllCompaniesCBD()
    all_country_count <- getAllCompanies()
    return(locq(consultancy_cbd_count, all_cbd_count, consultancy_country_count, all_country_count))
  })
  
  getConsultancyLQJur <- reactive({
    firms <- getFile()
    firms_consultancy <- firms[grep("Consultancy", firms$type), ]
    firms_consultancy <- firms_consultancy[!duplicated(firms_consultancy$postal_code),]
    consultancy_jur <- firms_consultancy[grep("Singapore 608|Singapore 609|Singapore 6001|Singapore 6002", firms_consultancy$postal_code), ]
    consultancy_jur_count <- nrow(consultancy_jur)
    all_jur_count <- getAllCompaniesJurong()
    consultancy_country_count <- nrow(firms_consultancy)
    all_country_count <- getAllCompanies()
    return(locq(consultancy_jur_count, all_jur_count, consultancy_country_count, all_country_count))
  })
  
  getAccountancyLQCBD <- reactive({
    firms <- getFile()
    firms_accountancy <- firms[grep("Accountancy", firms$type), ]
    firms_accountancy <- firms_accountancy[!duplicated(firms_accountancy$postal_code),]
    accountancy_cbd <- firms_accountancy[grep("Singapore 01|Singapore 02|Singapore 03|Singapore 04|Singapore 05|Singapore 06|Singapore 07|Singapore 08|Singapore 14|Singapore 15|Singapore 16|Singapore 09|Singapore 10|Singapore 11|Singapore 120|Singapore 13|Singapore 17|Singapore 18|Singapore 19|Singapore 20|Singapore 21|Singapore 22|Singapore 23|Singapore 24|Singapore 25|Singapore 26|Singapore 27|Singapore 28|Singapore 29|Singapore 30|Singapore 31|Singapore 32|Singapore 33|Singapore 34|Singapore 35|Singapore 36|Singapore 37|Singapore 38|Singapore 39|Singapore 40|Singapore 41|Singapore 42|Singapore 43|Singapore 44|Singapore 45|Singapore 57|Singapore 58|Singapore 59|Singapore 77", firms_accountancy$postal_code), ]
    accountancy_cbd_count <- nrow(accountancy_cbd)
    accountancy_country_count <- nrow(firms_accountancy)
    all_cbd_count <- getAllCompaniesCBD()
    all_country_count <- getAllCompanies()
    return(locq(accountancy_cbd_count, all_cbd_count, accountancy_country_count, all_country_count))
  })
  
  getAccountancyLQJur <- reactive({
    firms <- getFile()
    firms_accountancy <- firms[grep("Accountancy", firms$type), ]
    firms_accountancy <- firms_accountancy[!duplicated(firms_accountancy$postal_code),]
    accountancy_jur <- firms_accountancy[grep("Singapore 608|Singapore 609|Singapore 6001|Singapore 6002", firms_accountancy$postal_code), ]
    accountancy_jur_count <- nrow(accountancy_jur)
    all_jur_count <- getAllCompaniesJurong()
    accountancy_country_count <- nrow(firms_accountancy)
    all_country_count <- getAllCompanies()
    return(locq(accountancy_jur_count, all_jur_count, accountancy_country_count, all_country_count))
  })
  
  getArchitecturalLQCBD <- reactive({
    firms <- getFile()
    firms_architectural <- firms[grep("Architectural", firms$type), ]
    firms_architectural <- firms_architectural[!duplicated(firms_architectural$postal_code),]
    architectural_cbd <- firms_architectural[grep("Singapore 01|Singapore 02|Singapore 03|Singapore 04|Singapore 05|Singapore 06|Singapore 07|Singapore 08|Singapore 14|Singapore 15|Singapore 16|Singapore 09|Singapore 10|Singapore 11|Singapore 120|Singapore 13|Singapore 17|Singapore 18|Singapore 19|Singapore 20|Singapore 21|Singapore 22|Singapore 23|Singapore 24|Singapore 25|Singapore 26|Singapore 27|Singapore 28|Singapore 29|Singapore 30|Singapore 31|Singapore 32|Singapore 33|Singapore 34|Singapore 35|Singapore 36|Singapore 37|Singapore 38|Singapore 39|Singapore 40|Singapore 41|Singapore 42|Singapore 43|Singapore 44|Singapore 45|Singapore 57|Singapore 58|Singapore 59|Singapore 77", firms_architectural$postal_code), ]
    architectural_cbd_count <- nrow(architectural_cbd)
    architectural_country_count <- nrow(firms_architectural)
    all_cbd_count <- getAllCompaniesCBD()
    all_country_count <- getAllCompanies()
    return(locq(architectural_cbd_count, all_cbd_count, architectural_country_count, all_country_count))
  })
  
  getArchitecturalLQJur <- reactive({
    firms <- getFile()
    firms_architectural <- firms[grep("Architectural", firms$type), ]
    firms_architectural <- firms_architectural[!duplicated(firms_architectural$postal_code),]
    architectural_jur <- firms_architectural[grep("Singapore 608|Singapore 609|Singapore 6001|Singapore 6002", firms_architectural$postal_code), ]
    architectural_jur_count <- nrow(architectural_jur)
    all_jur_count <- getAllCompaniesJurong()
    architectural_country_count <- nrow(firms_architectural)
    all_country_count <- getAllCompanies()
    return(locq(architectural_jur_count, all_jur_count, architectural_country_count, all_country_count))
  })
  
  output$LQPlot <- renderPlot({
    values <- c(getLegalLQCBD(), getBankLQCBD(), getConsultancyLQCBD(), getAccountancyLQCBD(), getArchitecturalLQCBD())
    barplot(values,
            main = "Location Quotient in CBD",
            xlab = "Industry",
            ylab = "Location Quotient",
            names.arg = c("Legal", "Banks", "Consultancy", "Accountancy", "Architectural"),
            col = "darkred")
  })
  
  output$LQPlotJurong <- renderPlot({
    values <- c(getLegalLQJur(), getBankLQJur(), getConsultancyLQJur(), getAccountancyLQJur(), getArchitecturalLQJur())
    barplot(values,
            main = "Location Quotient in Jurong",
            xlab = "Industry",
            ylab = "Location Quotient",
            names.arg = c("Legal", "Banks", "Consultancy", "Accountancy", "Architectural"),
            col = "darkblue")
  })
  
  output$lqLegal <- renderUI({
    str3 <- paste("<b>Location Quotient in CBD:</b> ", getLegalLQCBD(), " ")
    str4 <- paste("<b>Location Quotient in Jurong:</b> ", getLegalLQJur(), " ")
    strl1 <- paste("<b>Number of Legal firms in CBD:</b> ", getLegalCount(), " ")
    strAll <- paste("<b>Number of firms in CBD:</b> ", getAllCompaniesCBD(), " ")
    strl2 <- paste("<b>Number of Legal firms in Jurong:</b> ", getLegalCountJur(), " ")
    strJur <- paste("<b>Number of firms in Jurong:</b> ", getAllCompaniesJurong(), " ")
    HTML(paste(str3, str4, strl1, strAll, strl2, strJur, sep = "<br/>"))
  })
  
  output$lqBank <- renderUI({
    strB1 <- paste("<b>Location Quotient in CBD:</b> ", getBankLQCBD(), " ")
    strB2 <- paste("<b>Location Quotient in Jurong:</b> ", getBankLQJur(), " ")
    strbnum1 <- paste("<b>Number of Banks in CBD:</b> ", getBanksCount(), " ")
    strAllBank <- paste("<b>Number of firms in CBD:</b> ", getAllCompaniesCBD(), " ")
    strbnum2 <- paste("<b>Number of Birms in Jurong:</b> ", getBanksCountJur(), " ")
    strJurBank <- paste("<b>Number of firms in Jurong:</b> ", getAllCompaniesJurong(), " ")
    HTML(paste(strB1, strB2, strbnum1, strAllBank, strbnum2, strJurBank, sep = "<br/>"))
  })
  
  output$lqConsultancy <- renderUI({
    strC1 <- paste("<b>Location Quotient in CBD:</b> ", getConsultancyLQCBD(), " ")
    strC2 <- paste("<b>Location Quotient in Jurong:</b> ", getConsultancyLQJur(), " ")
    strcnum1 <- paste("<b>Number of Consultancy Firms in CBD:</b> ", getConsultancyCount(), " ")
    strAllC <- paste("<b>Number of firms in CBD:</b> ", getAllCompaniesCBD(), " ")
    strcnum2 <- paste("<b>Number of Consultancy Firms in Jurong:</b> ", getConsultancyCountJur(), " ")
    strJurC <- paste("<b>Number of firms in Jurong:</b> ", getAllCompaniesJurong(), " ")
    HTML(paste(strC1, strC2, strcnum1, strAllC, strcnum2, strJurC, sep = "<br/>"))
  })
  
  output$lqAccountancy <- renderUI({
    strA1 <- paste("<b>Location Quotient in CBD:</b> ", getAccountancyLQCBD(), " ")
    strA2 <- paste("<b>Location Quotient in Jurong:</b> ", getAccountancyLQJur(), " ")
    stranum1 <- paste("<b>Number of Accounting Firms in CBD:</b> ", getAccountancyCount(), " ")
    strAllA <- paste("<b>Number of firms in CBD:</b> ", getAllCompaniesCBD(), " ")
    stranum2 <- paste("<b>Number Accounting Firms in Jurong:</b> ", getAccountancyCountJur(), " ")
    strJurA <- paste("<b>Number of firms in Jurong:</b> ", getAllCompaniesJurong(), " ")
    HTML(paste(strA1, strA2, stranum1, strAllA, stranum2, strJurA, sep = "<br/>"))
  })
  
  output$lqArchitectural <- renderUI({
    strAr1 <- paste("<b>Location Quotient in CBD:</b> ", getArchitecturalLQCBD(), " ")
    strAr2 <- paste("<b>Location Quotient in Jurong:</b> ", getArchitecturalLQJur(), " ")
    strarcnum1 <- paste("<b>Number of Architectural Firms in CBD:</b> ", getArchitecturalCount(), " ")
    strAllArc <- paste("<b>Number of firms in CBD:</b> ", getAllCompaniesCBD(), " ")
    strarcnum2 <- paste("<b>Number of Architectural Firms in Jurong:</b> ", getArchitecturalCountJur(), " ")
    strJurArc <- paste("<b>Number of firms in Jurong:</b> ", getAllCompaniesJurong(), " ")
    HTML(paste(strAr1, strAr2,strarcnum1, strAllArc, strarcnum2, strJurArc, sep = "<br/>"))
  })
  
  output$analysis <- renderUI({
    HTML(paste("<b>LQ Analysis</b>: <br/> If LQ < 1, the output is not sufficient to form a CBD and more firms of this sector are needed. It is also non-basic. <br/> If LQ > 1, the output is more than sufficient to form a CBD and exporting more firms is an option. It is basic."))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

